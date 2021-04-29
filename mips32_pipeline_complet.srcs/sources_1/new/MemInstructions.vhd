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

entity MemInstructions is
Port ( 
    i_addresse 		: in std_ulogic_vector (31 downto 0);
    o_instruction 	: out std_ulogic_vector (31 downto 0)
);
end MemInstructions;

architecture Behavioral of MemInstructions is
    signal ram_Instructions : RAM(0 to 255) := (
------------------------
-- Insérez votre code ici
------------------------
X"20080005",
X"20090003",
X"3c011001",
X"ac280000",
X"3c011001",
X"ac280004",
X"3c011001",
X"ac280008",
X"3c011001",
X"ac28000c",
X"3c011001",
X"ac290010",
X"3c011001",
X"ac290014",
X"3c011001",
X"ac290018",
X"3c011001",
X"ac29001c",


X"3c011001", -- LUI pour load l addresse de la ram ( 0x1001 0000 ) dans le registre $at
X"e4000030", -- LWV $vec0 0x10010000 

X"3c011001", -- LUI pour load l addresse de la ram ( 0x1001 0010 ) dans le registre $at
X"e4004050", -- LWV $vec1 0x10010010 

X"e0221802", -- ADDV $vec2 = $vec0 + $vec1

X"3c011001", -- LUI pour load l addresse de la ram ( 0x1001 0000 ) dans le registre $at
X"e4008078", -- SWV $vec2 0x10010020

X"e0412007", -- SLTUV $vec3 = $vec1 < $vec0

X"e0222807", -- SLTUV $vec4 = $vec0 < $vec1


X"e064300f", -- MOV $vec2 -> $vec5 if $vec3 = 1

X"e081380f", -- MOV $vec3 -> $vec6 if $vec0 = 1

------------------------
-- Fin de votre code
------------------------
    others => X"00000000"); --> SLL $zero, $zero, 0  

    signal s_MemoryIndex : integer range 0 to 255;

begin
    -- Conserver seulement l'indexage des mots de 32-bit/4 octets
    s_MemoryIndex <= to_integer(unsigned(i_addresse(9 downto 2)));

    -- Si PC vaut moins de 127, présenter l'instruction en mémoire
    o_instruction <= ram_Instructions(s_MemoryIndex) when i_addresse(31 downto 10) = (X"00400" & "00")
                    -- Sinon, retourner l'instruction nop X"00000000": --> AND $zero, $zero, $zero  
                    else (others => '0');

end Behavioral;
