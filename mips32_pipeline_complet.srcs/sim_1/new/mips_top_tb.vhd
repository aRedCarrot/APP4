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
use work.MIPS32_package.all;


entity mips_top_tb is
end mips_top_tb;

architecture Behavioral of mips_top_tb is

	component mips_pipeline_top is
	Port (
		clk 				: in std_ulogic;
		reset 				: in std_ulogic;
		o_pc 				: out std_ulogic_vector (31 downto 0);
		o_InstructionDebug 	: out std_ulogic_vector (64 downto 0);
		o_zero 				: out std_ulogic
		);
	end component;

    constant clk_cycle : time := 10 ns;
    signal clk : std_ulogic;
    signal reset : std_ulogic;
	signal s_InstructionDebug: std_ulogic_vector (64 downto 0);
	signal debug_Instruction_IF  : op_type;
	signal debug_Instruction_ID  : op_type;
	signal debug_Instruction_EX  : op_type;
	signal debug_Instruction_MEM : op_type;
	signal debug_Instruction_WB  : op_type;
	
begin

debug_Instruction_IF  <= f_DisplayOp(s_InstructionDebug(12 downto  0));
debug_Instruction_ID  <= f_DisplayOp(s_InstructionDebug(25 downto 13));
debug_Instruction_EX  <= f_DisplayOp(s_InstructionDebug(38 downto 26));
debug_Instruction_MEM <= f_DisplayOp(s_InstructionDebug(51 downto 39));
debug_Instruction_WB  <= f_DisplayOp(s_InstructionDebug(64 downto 52));


process
begin
    reset <= '1';
    wait for clk_cycle * 4;
    wait for clk_cycle / 5; -- optionnel: relâcher le reset juste après le front d'horloge
    reset <= '0';
    wait;
end process;

process
begin
    clk <= '1';
    loop
        wait for clk_cycle/2;
        clk <= not clk;
    end loop;
end process;

dut : mips_pipeline_top 
Port map( 
	clk => clk,
	reset => reset,
	o_InstructionDebug => s_InstructionDebug,
	o_pc => open,
	o_zero => open
	);

end Behavioral;
