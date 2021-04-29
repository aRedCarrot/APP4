----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/02/2020 09:01:55 PM
-- Design Name: 
-- Module Name: hazard_controller - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity hazard_controller is
    Port ( 
    clk         : in STD_ULOGIC;
    i_IFID_rs   : in STD_LOGIC_VECTOR(4 downto 0);
    i_IFID_rt   : in STD_LOGIC_VECTOR(4 downto 0);
    i_IDEX_RD   : in STD_LOGIC_VECTOR(4 downto 0);
    i_IDEX_RT   : in STD_LOGIC_VECTOR(4 downto 0);
    i_EXMEM_RD  : in STD_LOGIC_VECTOR(4 downto 0);
    i_EXMEM_RT  : in STD_LOGIC_VECTOR(4 downto 0);
    i_MEMWB_RD  : in STD_LOGIC_VECTOR(4 downto 0);
    i_MEMWB_RT  : in STD_LOGIC_VECTOR(4 downto 0);
    i_OP_Code   : in STD_LOGIC_VECTOR(5 downto 0);
    
    o_stall     : out STD_LOGIC
    );
end hazard_controller;

architecture Behavioral of hazard_controller is
    SIGNAL s_Ancien0        : STD_LOGIC     := '0'; --La lecture actuelle
    SIGNAL s_Ancien1        : STD_LOGIC     := '0';
    SIGNAL s_Ancien2        : STD_LOGIC     := '0';
    SIGNAL s_Ancien3        : STD_LOGIC     := '0';
    SIGNAL s_NotFirstTime   : STD_LOGIC     := '0';
    SIGNAL s_stall          : STD_ULOGIC    := '0';
begin

    PROCESS(clk) IS
    BEGIN
        IF(FALLING_EDGE(clk)) THEN
            --s_stall <= '0';
            --Mémoire des quatre dernières opérations, 0 est une opération R et 1 est une opération I
            s_Ancien3 <= s_Ancien2;
            s_Ancien2 <= s_Ancien1;
            s_Ancien1 <= s_Ancien0;
            IF(i_OP_Code = "000100" OR i_OP_Code = "001000" OR i_OP_Code = "001101" OR i_OP_Code = "001111" OR i_OP_Code = "100011" OR i_OP_Code = "101011") THEN s_Ancien0 <= '1';
            ELSE s_Ancien0 <= '0';
            END IF;
        END IF;
    END PROCESS;

    checkForStalls  :   PROCESS(clk)
    BEGIN
        s_stall <= '0';
        --Définition d'un stall
        IF((i_IFID_rs /= "00000" OR i_IFID_rt /= "00000") OR s_NotFirstTime = '1') THEN
            IF((i_IDEX_RD /= "00000" AND s_Ancien1 = '0') OR (i_IDEX_RT /= "00000" AND s_Ancien1 = '1')) THEN
                --
                IF(s_Ancien0 = '0' AND s_Ancien1 = '0') THEN
                    IF(i_IFID_rs = i_IDEX_RD OR i_IFID_rt = i_IDEX_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '0' AND s_Ancien1 = '1') THEN
                    IF(i_IFID_rs = i_IDEX_RT OR i_IFID_rt = i_IDEX_RT) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien1 = '0') THEN
                    IF(i_IFID_rs = i_IDEX_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien1 = '1') THEN
                    IF(i_IFID_rs = i_IDEX_RT) THEN
                        s_stall <= '1';
                    END IF;
                END IF;
                --
            END IF;
            IF((i_EXMEM_RD /= "00000" AND s_Ancien2 = '0') OR (i_EXMEM_RT /= "00000" AND s_Ancien2 = '1')) THEN
                --
                IF(s_Ancien0 = '0' AND s_Ancien2 = '0') THEN
                    IF(i_IFID_rs = i_EXMEM_RD OR i_IFID_rt = i_EXMEM_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '0' AND s_Ancien2 = '1') THEN
                    IF(i_IFID_rs = i_EXMEM_RT OR i_IFID_rt = i_EXMEM_RT) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien2 = '0') THEN
                    IF(i_IFID_rs = i_EXMEM_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien2 = '1') THEN
                    IF(i_IFID_rs = i_EXMEM_RT) THEN
                        s_stall <= '1';
                    END IF;
                END IF;
                --
            END IF;
            IF((i_MEMWB_RD /= "00000" AND s_Ancien3 = '0') OR (i_MEMWB_RT /= "00000" AND s_Ancien3 = '1')) THEN
                --
                IF(s_Ancien0 = '0' AND s_Ancien3 = '0') THEN
                    IF(i_IFID_rs = i_MEMWB_RD OR i_IFID_rt = i_MEMWB_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '0' AND s_Ancien3 = '1') THEN
                    IF(i_IFID_rs = i_MEMWB_RT OR i_IFID_rt = i_MEMWB_RT) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien3 = '0') THEN
                    IF(i_IFID_rs = i_MEMWB_RD) THEN
                        s_stall <= '1';
                    END IF;
                ELSIF(s_Ancien0 = '1' AND s_Ancien3 = '1') THEN
                    IF(i_IFID_rs = i_MEMWB_RT) THEN
                        s_stall <= '1';
                    END IF;
                END IF;
                --
            END IF;
            s_NotFirstTime <= '1';
        END IF;
        --Fin de définition d'un stall
    END PROCESS;
    
    executeStall    :   PROCESS(s_stall)
    BEGIN
        IF(s_stall = '0') THEN
            o_stall <= '0';
        ELSE
            o_stall <= '1';
        END IF;
    END PROCESS;

end Behavioral;
