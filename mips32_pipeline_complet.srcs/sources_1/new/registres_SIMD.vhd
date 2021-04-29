----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/04/2020 01:39:08 PM
-- Design Name: 
-- Module Name: registres_SIMD - Behavioral
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
use ieee.numeric_std.all;
use work.MIPS32_package.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity registres_SIMD is
    Port (     clk       : in  std_ulogic;
               reset     : in  std_ulogic;
               i_RS1     : in  std_ulogic_vector (4 downto 0);
               i_RS2     : in  std_ulogic_vector (4 downto 0);
               i_Wr_DAT  : in  std_ulogic_vector (127 downto 0);
               i_WDest   : in  std_ulogic_vector (4 downto 0);
               i_WE 	 : in  std_ulogic;
               o_RS1_DAT : out std_ulogic_vector (127 downto 0);
               o_RS2_DAT : out std_ulogic_vector (127 downto 0)
               );
end registres_SIMD;

architecture Behavioral of registres_SIMD is
    signal regs: RAM_SIMD(0 to 31) := (others => (others => '0')); -- Cree 32 registres de 128 bits
begin

    process( clk )
    begin
        if clk='1' and clk'event then
            if i_WE = '1' and reset = '0' and i_WDest /= "00000" then
                regs( to_integer( unsigned(i_WDest))) <= i_Wr_DAT;
            end if;
        end if;
    end process;
    
    o_RS1_DAT <= regs( to_integer(unsigned(i_RS1)));
    o_RS2_DAT <= regs( to_integer(unsigned(i_RS2)));

end Behavioral;
