-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Rostislav Kral <xkralr06 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

-- START PTR
signal ptr_inc		        :std_logic;
signal ptr_dec		        :std_logic;
signal ptr_register		    :std_logic_vector(12 downto 0) := "1000000000000";
signal c                    :std_logic_vector(12 downto 0);
-- END PTR



-- START PC
signal pc_reg		        :std_logic_vector(12 downto 0);
signal pc_inc	            :std_logic;
signal pc_dec		        :std_logic;
signal pc_help              :std_logic;
-- END PC


-- STARTMUX2
signal mux2_sel              :std_logic_vector (1 downto 0) := "00";
signal mux2_out              :std_logic_vector(7 downto 0) := "00000000";
--  END MUX2

type s_states is (
    s_start,
    s_fetch,
    s_decode,

    s_ptr_increment,
    s_ptr_decrement,

    s_value_increment_read,
    s_value_increment_write,

    s_value_decrement_read,
    s_value_decrement_write,

    s_while_read,
    s_do_while_right,
    s_do_while_left,

    s_while_start_state,
    s_while_left_check,
    s_while_left_load,
    s_while_left_not_found,
    s_while_left_start_state,

    s_while_right_state,
    s_while_right,
    s_while_right_continue,
    s_while_right_check,

    s_output_read,
    s_output_write,

    s_input_read,
    s_input_write,

   

    s_null
);



signal currentState: s_states := s_start;
signal nextState: s_states := s_start;



--START MUX1
signal mux1Sel              :std_logic;
signal mux1Out              :std_logic_vector(12 downto 0 ) := "0000000000000";
-- END MUX1


begin

-- START PC
    pc: process (CLK, RESET, pc_inc, pc_help, pc_dec) is
		begin 
            if(RESET = '1') then
                pc_reg <= "0000000000000";
            elsif rising_edge(CLK) then
                if(pc_inc = '1') then
                    pc_reg <= pc_reg + 1;
                elsif (pc_dec = '1') then
                    pc_reg <= pc_reg - 1;
                elsif (pc_help = '1') then
                    pc_reg <= c;
                end if;
            end if;        
		end process;

-- END PC


-- START MUX1
MUX1: process(CLK, RESET, mux1Sel,pc_help, pc_reg) is
begin
    if RESET = '1' then
        mux1Out <= (others => '0');
    elsif (CLK'event) then
        case mux1Sel is
            when '0' =>
                mux1Out <= pc_reg;
            when '1' =>
                mux1Out <= ptr_register;
            when others => 
        end case;
    end if;
end process;
DATA_ADDR <= mux1Out;

-- END MUX1

-- START PTR
    ptr: process (CLK, RESET, ptr_inc, ptr_dec) is
    begin 
        if(RESET = '1') then
            ptr_register <= "1000000000000";
        elsif rising_edge(CLK) then
            if(ptr_inc = '1') then
                if ptr_register = "1111111111111" then
                    ptr_register <= "1000000000000";
                else
                    ptr_register <= ptr_register + 1;
                end if;
            elsif (ptr_dec = '1') then
                if ptr_register = "1000000000000" then
                    ptr_register <= "1111111111111";
                else
                    ptr_register <= ptr_register - 1;
                end if;
            end if;
        end if;        
    end process;

-- END PTR


-- START MUX2
    MUX2: process(CLK, RESET, mux2_sel, IN_DATA, DATA_RDATA) is
        begin
            if RESET = '1' then
                mux2_out <= (others => '0');
            elsif (CLK'event) then
                case mux2_sel is
                    when "10" =>
					    mux2_out <= DATA_RDATA + 1;
                    when "01" => 
                        mux2_out <= DATA_RDATA - 1;
                    when "00" =>
                        mux2_out <= IN_DATA;
                    when others => 
                end case;
            end if;
        end process;
        DATA_WDATA <= mux2_out;

-- END MUX2

-- START FSM
    STATES_CHANGE: process (CLK, RESET, EN)
        begin
            if RESET = '1' then
                currentState <= s_start;
            elsif CLK'event and CLK = '1' then
                if EN = '1' then
                    currentState <= nextState;
                end if;
            end if;
    end process;

        FSM: process (currentState, DATA_RDATA, OUT_BUSY, IN_VLD)
        begin
                

                pc_inc <= '0';
                pc_dec <= '0';
                ptr_inc <= '0';
                ptr_dec <= '0';
                pc_help <= '0';



                mux2_sel <= "00";

                DATA_EN <= '0';
                DATA_RDWR <= '0';
                IN_REQ  <= '0';
                OUT_WE <= '0';




                case currentState is
                    when s_start =>
                        mux1Sel <= '0';
                        nextState <= s_fetch;
                    when s_fetch =>
                        mux1Sel <= '0';
                        DATA_EN <= '1';
                        nextState <= s_decode;
                    when s_decode =>
                    mux1Sel <= '0';
                        case DATA_RDATA is
                            when X"3E" => 
                                nextState <= s_ptr_increment;
                            when X"3C" =>  
                                nextState <= s_ptr_decrement;
                            when X"2B" => 
                                nextState <= s_value_increment_read;
                            when X"2D" =>   
                                nextState <= s_value_decrement_read;
                            when X"5B" =>   
                                nextState <= s_while_start_state;
                             when X"28" =>  
                                nextState <= s_do_while_left;
                            when X"29" => 
                                nextState <= s_while_read;
                            when X"5D" =>   
                                nextState <= s_while_right_state;
                            when X"2E" =>   
                                nextState <= s_output_read;
                            when X"2C" =>  
                                nextState <= s_input_read;
                            when X"00" =>   
                                nextState <= s_null;
                            when others =>
                                --nextState <= s_fetch;
                        end case;
                when s_ptr_increment =>
                        pc_inc <= '1';
                        ptr_inc <= '1';
                        nextState <= s_fetch;
                when s_ptr_decrement =>
                        pc_inc <= '1';
                        ptr_dec <= '1';
                        nextState <= s_fetch;
                when s_value_increment_read =>
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        mux1Sel <= '1';
                        nextState <= s_value_increment_write;
                when s_value_increment_write =>
                        DATA_EN <= '1';
                        DATA_RDWR <= '1';
                        mux2_sel <= "10";
                        pc_inc <= '1';
                        nextState <= s_fetch;
                when s_value_decrement_write =>
                        DATA_EN <= '1';
                        DATA_RDWR <= '1';
                        pc_inc <= '1';
                        mux2_sel <= "01";
                        nextState <= s_fetch;
                when s_value_decrement_read =>
                        DATA_RDWR <= '0';
                        DATA_EN <= '1';
                        mux1Sel <= '1';
                        nextState <= s_value_decrement_write;
                when s_while_start_state =>
                        mux1Sel <= '1';
                        nextState <= s_while_left_start_state;
                when s_while_left_start_state =>
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        pc_inc <= '1';
                        nextState <= s_while_left_check;
                when s_while_left_check =>
                    if DATA_RDATA = "00000000" then
                        mux1Sel <= '0';
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        nextState <= s_while_left_load;
                    else
                        nextState <= s_fetch;
                    end if;
                when s_while_left_load =>
                    if DATA_RDATA /= X"5D" then
                        pc_inc <= '1';
                        nextState <= s_while_left_not_found;
                    else
                        pc_inc <= '1';
                        nextState <= s_fetch;
                    end if;
                when s_while_left_not_found =>
                        mux1Sel <= '0';
                        DATA_RDWR <= '0';
                        nextState <= s_while_left_load;
                when s_while_right_check =>
                        DATA_EN <= '1';
                        if DATA_RDATA /= "00000000" then
                            mux1Sel <= '0';
                            pc_dec <= '1';
                            nextState <= s_while_right;
                        else
                            pc_inc <= '1';
                            nextState <= s_fetch;
                        end if;        
                when s_while_right_state =>
                        mux1Sel <= '1';
                        DATA_RDWR <= '0';
                        DATA_EN <= '1';
                        nextState <= s_while_right_check;
                when s_while_right =>
                        mux1Sel <= '0';
                        DATA_RDWR <= '0';
                        DATA_EN <= '1';
                        nextState <= s_while_right_continue;
                when s_while_right_continue =>
                        if DATA_RDATA /= X"5B" then
                            pc_dec <= '1';
                            nextState <= s_while_right;
                        else
                            pc_inc <= '1';
                            nextState <= s_fetch;
                        end if;
                when s_do_while_left =>
                    pc_inc <= '1';
                    c <= pc_reg + 1;
                    nextState <= s_fetch;
                when s_while_read =>
                        mux1Sel <= '1';
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        pc_inc <= '1';
                        nextState <= s_do_while_right;
                when s_do_while_right =>
                    nextState <= s_fetch;
                    if DATA_RDATA /= "00000000" then
                        pc_help <= '1';
                        nextState <= s_fetch;
                    end if;
                when s_input_write =>
                    if IN_VLD /= '1' then
                        IN_REQ <= '1';
                        mux1Sel <= '1';
                        mux2_sel <= "00";
                        nextState <= s_input_read;
                    else
                        DATA_EN <= '1';
                        DATA_RDWR <= '1';
                        IN_REQ <= '0';
                        pc_inc <= '1';
                        nextState <= s_fetch;
                    end if;
                when s_output_read =>
                    if OUT_BUSY /= '0' then
                        nextState <= s_output_read;
                    else
                        DATA_EN <= '1';
                        mux1Sel <= '1';
                        DATA_RDWR <= '0';
                        nextState <= s_output_write;
                    end if;
                when s_output_write =>
                    OUT_WE <= '1';
                    pc_inc <= '1';
                    OUT_DATA <= DATA_RDATA;
                    mux1Sel <= '1';
                    nextState <= s_fetch;
                when s_input_read =>
                    nextState <= s_input_write;
                    mux2_sel <= "00";
                    IN_REQ <= '1';
                    mux1Sel <= '1';
                when s_null =>
                when others =>
                    pc_inc <= '1';
                    end case;
            end process;
        end behavioral;