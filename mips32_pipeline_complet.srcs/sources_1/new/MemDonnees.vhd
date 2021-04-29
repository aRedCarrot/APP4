---------------------------------------------------------------------------------------------
--
--	Université de Sherbrooke 
--  Département de génie électrique et génie informatique
--
--	S4i - APP4 
--	
--
--	Auteur: 		Marc-André Tétrault
--					Daniel Dalle
--					Sébastien Roy
-- 
---------------------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all; -- requis pour la fonction "to_integer"
use work.MIPS32_package.all;

entity MemDonnees is
Port ( 
	clk 		: in std_ulogic;
	reset 		: in std_ulogic;
	i_MemWrite 	: in std_ulogic;
    i_Addresse 	: in std_ulogic_vector (31 downto 0);
	i_WriteData : in std_ulogic_vector (31 downto 0);
	
	i_enable_SIMD : in std_ulogic;
	i_WriteData_SIMD : in std_ulogic_vector(127 downto 0);
	
    o_ReadData 	: out std_ulogic_vector (31 downto 0);
    o_ReadData_SIMD : out std_ulogic_vector(127 downto 0)
);
end MemDonnees;

architecture Behavioral of MemDonnees is
    signal ram_DataMemory : RAM(0 to 255) := ( -- type défini dans le package
------------------------
-- Insérez vos donnees ici
------------------------
X"12345678",
X"87654321",
X"bad0face",
X"00000001",
X"00000002",
X"00000003",
X"00000004",
X"00000005",
X"00000006",
X"5555cccc",
------------------------
-- Fin de votre code
------------------------
    others => X"00000000");

    signal s_MemoryIndex 	: integer range 0 to 255; -- 0-127
	signal s_WriteMapValid 	: std_ulogic;

begin
    -- Transformation de l'adresse en entier à interval fixés
    s_MemoryIndex 	<= to_integer(unsigned(i_Addresse(9 downto 2)));
	s_WriteMapValid <= '1' when i_Addresse(31 downto 10) = (X"10010" & "00") else '0'; 
	
	
	-- Partie pour l'écriture
	process( clk )
    begin
        if clk='1' and clk'event then
            if i_MemWrite = '1' and reset = '0' and s_WriteMapValid = '1' then
                if( i_enable_SIMD = '1') THEN
                    ram_DataMemory(s_MemoryIndex) <= i_WriteData_SIMD(127 downto 96);
                    ram_DataMemory(s_MemoryIndex + 1) <= i_WriteData_SIMD(95 downto 64);
                    ram_DataMemory(s_MemoryIndex + 2) <= i_WriteData_SIMD(63 downto 32);
                    ram_DataMemory(s_MemoryIndex + 3) <= i_WriteData_SIMD(31 downto 0);
                ELSE 
                    ram_DataMemory(s_MemoryIndex) <= i_WriteData;
                END IF;
            end if;
        end if;
    end process;

    -- Valider que nous sommes dans le segment de mémoire, avec 256 addresses valides
    o_ReadData <= ram_DataMemory(s_MemoryIndex) when s_WriteMapValid = '1'
                    -- Sinon, retourner l'instruction X"00000000": --> AND $zero, $zero, $zero  
                    else (others => '0');
                    
    o_ReadData_SIMD <= ram_DataMemory(s_MemoryIndex) & ram_DataMemory(s_MemoryIndex + 1) & ram_DataMemory(s_MemoryIndex + 2) & ram_DataMemory(s_MemoryIndex + 3)
                when s_WriteMapValid = '1' AND i_enable_SIMD = '1'
                else (others => '0');

end Behavioral;
