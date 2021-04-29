


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;


entity mips_pipeline_top is
Port ( 
	clk 			: in std_ulogic;
	reset 			: in std_ulogic;
	o_pc 			: out std_ulogic_vector (31 downto 0);
	o_InstructionDebug : out std_ulogic_vector (64 downto 0);
	o_zero 			: out std_ulogic
	);
end mips_pipeline_top;

architecture Behavioral of mips_pipeline_top is

component controlleur is
Port (
    i_Op        	: in std_ulogic_vector(5 downto 0);
    i_funct_field 	: in std_ulogic_vector (5 downto 0);
    
    o_RegDst    	: out std_ulogic;
    o_Branch    	: out std_ulogic;
    o_MemRead   	: out std_ulogic;
    o_MemtoReg  	: out std_ulogic;
    o_AluFunct  	: out std_ulogic_vector (3 downto 0);
    o_MemWrite  	: out std_ulogic;
    o_ALUSrc    	: out std_ulogic;
    o_RegWrite  	: out std_ulogic;
	
	-- Sorties supp. vs 4.17
    o_Jump 			: out std_logic;
	o_jump_register : out std_logic;
	o_jump_link 	: out std_logic;
	o_SignExtend 	: out std_ulogic;
	
	o_isSimd     : out std_ulogic;
    o_SIMD_type  : out std_ulogic_vector(5 downto 0);
     
    o_Err       	: out std_ulogic
    );
end component;

component mips_datapath_pipeline is
Port ( 
	clk 			: in std_ulogic;
	reset 			: in std_ulogic;

	i_alu_funct   	: in std_ulogic_vector(3 downto 0);
	i_RegWrite    	: in std_ulogic;
	i_RegDst      	: in std_ulogic;
	i_MemtoReg    	: in std_ulogic;
	i_branch      	: in std_ulogic;
	i_ALUSrc      	: in std_ulogic;
	i_MemWrite	  	: in std_ulogic;

	i_jump   	  	: in std_ulogic;
	i_jump_register : in std_ulogic;
	i_jump_link   	: in std_ulogic;
	i_SignExtend 	: in std_ulogic;
	
	i_isSimd        : in std_ulogic;
	i_SIMD_type     : in std_ulogic_vector(5 downto 0);

	o_Instruction 		: out std_ulogic_vector (31 downto 0);
	o_InstructionDebug 	: out std_ulogic_vector (64 downto 0);
	o_PC 				: out std_ulogic_vector (31 downto 0)
);
end component;

    signal s_alu_funct   	: std_ulogic_vector(3 downto 0);
    signal s_RegWrite    	: std_ulogic;
	signal s_RegDst      	: std_ulogic;
    signal s_MemtoReg    	: std_ulogic;
	signal s_branch      	: std_ulogic;
    signal s_ALUSrc      	: std_ulogic;
	signal s_MemWrite		: std_logic;
	signal s_jump_register 	: std_ulogic;
	signal s_jump_link 		: std_ulogic;
    signal s_jump   		: std_ulogic;
    signal s_SignExtend   	: std_ulogic;
    
    signal s_isSimd         : std_ulogic;
    signal s_SIMD_type      : std_ulogic_vector(5 downto 0);

	
    signal s_Instruction 	: std_ulogic_vector(31 downto 0);
    -- champs du registre d'instructions
    alias s_instr_funct		: std_ulogic_vector(5 downto 0) is s_Instruction(5 downto 0);
    alias s_opcode			: std_ulogic_vector(5 downto 0) is s_Instruction(31 downto 26);

begin

-- Contrôles
inst_Controlleur: controlleur
Port map(
    i_Op            => s_opcode,
    i_funct_field   => s_instr_funct,
    
    o_RegDst    => s_RegDst,
    o_Branch    => s_branch,
    o_MemRead   => open,
    o_MemtoReg  => s_MemtoReg,
    o_AluFunct  => s_alu_funct,
    o_MemWrite  => s_MemWrite,
    o_ALUSrc    => s_ALUSrc,
    o_RegWrite  => s_RegWrite,
	
	-- Sorties supp. vs 4.17
    o_Jump => s_Jump,
	o_jump_register => s_jump_register,
	o_jump_link => s_jump_link,
	o_SignExtend	=> s_SignExtend,
	
	o_isSimd        => s_isSimd,
	o_SIMD_type     => s_SIMD_type,
     
    -- pour aider le debug
    o_Err       => open
    );
	
	
	
-- Chemin de données
inst_Datapath :  mips_datapath_pipeline
Port map( 
	clk 			=> clk,
	reset 			=> reset,

	i_alu_funct   	=> s_alu_funct,
	i_RegWrite    	=> s_RegWrite,
	i_RegDst      	=> s_RegDst,
	i_MemtoReg    	=> s_MemtoReg,
	i_branch      	=> s_branch,
	i_ALUSrc      	=> s_ALUSrc,
	i_MemWrite	  	=> s_MemWrite,
	i_jump   	  	=> s_jump,
	i_jump_register => s_jump_register,
	i_jump_link   	=> s_jump_link,
	i_SignExtend	=> s_SignExtend,
	i_isSimd        => s_isSimd,
	i_SIMD_type     => s_SIMD_type,
	o_Instruction 	=> s_Instruction,
	o_InstructionDebug	=> o_InstructionDebug, -- dans registre...
	o_PC				=> o_PC
);	

        
end Behavioral;
