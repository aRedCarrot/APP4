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
use ieee.numeric_std.all;
use work.MIPS32_package.all;


entity mips_datapath_pipeline is
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
	o_InstructionDebug 	: out std_ulogic_vector (64 downto 0); -- (6 + 6 + 1) * 5 
	o_PC		 		: out std_ulogic_vector (31 downto 0)
);
end mips_datapath_pipeline;

architecture Behavioral of mips_datapath_pipeline is


component MemInstructions is
    Port ( i_addresse : in std_ulogic_vector (31 downto 0);
           o_instruction : out std_ulogic_vector (31 downto 0));
end component;

component MemDonnees is
Port ( 
	clk : in std_ulogic;
	reset : in std_ulogic;
	i_MemWrite : in std_ulogic;
    i_Addresse : in std_ulogic_vector (31 downto 0);
	i_WriteData : in std_ulogic_vector (31 downto 0);
	
	i_enable_SIMD : in std_ulogic;
	i_WriteData_SIMD : in std_ulogic_vector(127 downto 0);
	
    o_ReadData : out std_ulogic_vector (31 downto 0);
    o_ReadData_SIMD : out std_ulogic_vector(127 downto 0)
);
end component;

component BancRegistres is
    Port ( clk : in std_ulogic;
           reset : in std_ulogic;
           i_RS1 : in std_ulogic_vector (4 downto 0);
           i_RS2 : in std_ulogic_vector (4 downto 0);
           i_Wr_DAT : in std_ulogic_vector (31 downto 0);
           i_WDest : in std_ulogic_vector (4 downto 0);
           i_WE : in std_ulogic;
           o_RS1_DAT : out std_ulogic_vector (31 downto 0);
           o_RS2_DAT : out std_ulogic_vector (31 downto 0));
end component;

COMPONENT registres_SIMD is
    Port (     clk       : in  std_ulogic;
               reset     : in  std_ulogic;
               i_RS1     : in  std_ulogic_vector (4 downto 0);
               i_RS2     : in  std_ulogic_vector (4 downto 0);
               i_Wr_DAT  : in  std_ulogic_vector (127 downto 0);
               i_WDest   : in  std_ulogic_vector (4 downto 0);
               i_WE 	 : in  std_ulogic;
               o_RS1_DAT : out std_ulogic_vector (127 downto 0);
               o_RS2_DAT : out std_ulogic_vector (127 downto 0));
end COMPONENT;

component alu is
    Port ( i_a : in std_ulogic_vector (31 downto 0);
           i_b : in std_ulogic_vector (31 downto 0);
           i_alu_funct: in std_ulogic_vector (3 downto 0);
		   i_shamt : in std_ulogic_vector (4 downto 0);
		   i_isSimd : in std_ulogic;
           o_result : inout std_ulogic_vector (31 downto 0);
           o_v : out std_ulogic;
           o_zero : out std_ulogic);
end component;

component hazard_controller is
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
    
    o_stall : out STD_LOGIC
    );
end component;

	constant c_Registre31		 : std_ulogic_vector(4 downto 0) := "11111";
	constant c_Nop				 : std_ulogic_vector(31 downto 0) := (others => '0');
	signal s_zero        : std_ulogic;
	
    signal s_WriteRegDest_muxout: std_ulogic_vector(4 downto 0);
	
    signal r_PC                    : std_ulogic_vector(31 downto 0);
    signal s_PC_Suivant            : std_ulogic_vector(31 downto 0);
    signal s_adresse_PC_plus_4     : std_ulogic_vector(31 downto 0);
    signal s_adresse_jump          : std_ulogic_vector(31 downto 0);
    signal s_adresse_branche       : std_ulogic_vector(31 downto 0);
    
    
    signal s_Data2Reg_muxout       : std_ulogic_vector(31 downto 0);
    
    signal s_reg_data1        : std_ulogic_vector(31 downto 0);
    signal s_reg_data2        : std_ulogic_vector(31 downto 0);
    signal s_imm_extended          : std_ulogic_vector(31 downto 0);
    signal s_imm_extended_shifted  : std_ulogic_vector(31 downto 0);
	
    signal s_Reg_Wr_Data           : std_ulogic_vector(31 downto 0);
    signal s_AluResult             : std_ulogic_vector(31 downto 0);
    signal s_MemoryReadData        : std_ulogic_vector(31 downto 0);
    signal s_AluB_data             : std_ulogic_vector(31 downto 0);
    signal s_AluA_data             : std_ulogic_vector(31 downto 0);
	
	
    signal s_Instruction           : std_ulogic_vector(31 downto 0);
    -- champs du mot d'instructions
    alias s_opcode                 : std_ulogic_vector( 5 downto 0) is s_Instruction(31 downto 26);
    alias s_rs                     : std_ulogic_vector( 4 downto 0) is s_Instruction(25 downto 21);
    alias s_rt                     : std_ulogic_vector( 4 downto 0) is s_Instruction(20 downto 16);
    alias s_rd                     : std_ulogic_vector( 4 downto 0) is s_Instruction(15 downto 11);
    alias s_shamt                  : std_ulogic_vector( 4 downto 0) is s_Instruction(10 downto  6);
    alias s_instr_funct            : std_ulogic_vector( 5 downto 0) is s_Instruction( 5 downto  0);
    alias s_imm16                  : std_ulogic_vector(15 downto 0) is s_Instruction(15 downto  0);
    alias s_jump_field             : std_ulogic_vector(25 downto 0) is s_Instruction(25 downto  0);
	
	
    signal r_ID_Instruction     : std_ulogic_vector(31 downto 0);
    signal s_ID_Instruction_mux     : std_ulogic_vector(31 downto 0);-- indice pour les nops
    -- champs du mot d'instructions
    alias s_ID_opcode                 : std_ulogic_vector( 5 downto 0) is s_ID_Instruction_mux(31 downto 26);
    signal s_ID_rs                     : std_ulogic_vector( 4 downto 0) ;--is s_ID_Instruction_mux(25 downto 21);
    signal s_ID_rt                     : std_ulogic_vector( 4 downto 0) ;--is s_ID_Instruction_mux(20 downto 16);
    alias s_ID_rd                     : std_ulogic_vector( 4 downto 0) is s_ID_Instruction_mux(15 downto 11);
    alias s_ID_shamt                  : std_ulogic_vector( 4 downto 0) is s_ID_Instruction_mux(10 downto  6);
    alias s_ID_instr_funct            : std_ulogic_vector( 5 downto 0) is s_ID_Instruction_mux( 5 downto  0);
    alias s_ID_imm16                  : std_ulogic_vector(15 downto 0) is s_ID_Instruction_mux(15 downto  0);
    alias s_ID_jump_field             : std_ulogic_vector(25 downto 0) is s_ID_Instruction_mux(25 downto  0);
	
	
    signal r_ID_adresse_PC_plus_4     : std_ulogic_vector(31 downto 0);
	
	
	-- Registres étage EX
	-- Contrôles
	-- EX stage endpoint
	signal r_EX_alu_funct   	: std_ulogic_vector(3 downto 0);
	signal r_EX_ALUSrc      	: std_ulogic;
	signal r_EX_RegDst      	: std_ulogic;
	-- MEM stage endpoint
	signal r_EX_branch      	: std_ulogic;
	signal r_EX_MemWrite	  	: std_logic;
	signal r_EX_jump   	  	: std_ulogic;
	signal r_EX_jump_register 	: std_ulogic;
    signal r_EX_jump_field          : std_ulogic_vector(25 downto 0);
	-- WB stage endpoint
	signal r_EX_jump_link   	: std_ulogic;
	signal r_EX_RegWrite    	: std_ulogic;
	signal r_EX_MemtoReg    	: std_ulogic;
    signal r_EX_adresse_PC_plus_4     : std_ulogic_vector(31 downto 0);
	
	-- Signaux générés
    signal r_EX_imm_extended          : std_ulogic_vector(31 downto 0);
	signal r_EX_reg_data1		      : std_ulogic_vector(31 downto 0);
	signal r_EX_reg_data2		      : std_ulogic_vector(31 downto 0);
	signal r_EX_rt				: std_ulogic_vector(4 downto 0);
	signal r_EX_rd				: std_ulogic_vector(4 downto 0);
	signal r_EX_shamt			: std_ulogic_vector(4 downto 0);
	
	-- Registres étage MEM
	-- MEM stage endpoint
	signal r_MEM_branch      	: std_ulogic;
	signal r_MEM_MemWrite	  	: std_logic;
	signal r_MEM_jump   	  	: std_ulogic;
	signal r_MEM_jump_register 	: std_ulogic;
	-- WB stage endpoint
	signal r_MEM_jump_link   	: std_ulogic;
	signal r_MEM_RegWrite    	: std_ulogic;
	signal r_MEM_MemtoReg    	: std_ulogic;
    signal r_MEM_adresse_PC_plus_4     : std_ulogic_vector(31 downto 0);
	
	
	-- Signaux générés
    signal r_MEM_imm_extended          : std_ulogic_vector(31 downto 0);
    signal r_MEM_imm_extended_shifted  : std_ulogic_vector(31 downto 0);
	signal r_MEM_AluResult				   : std_ulogic_vector(31 downto 0);
	signal r_MEM_zero					: std_ulogic;
	signal r_MEM_reg_data1			      : std_ulogic_vector(31 downto 0);
	signal r_MEM_reg_data2			      : std_ulogic_vector(31 downto 0);
	signal r_MEM_WriteRegDest_muxout	: std_ulogic_vector(4 downto 0);
	
	-- Registres étage MEM
	-- WB stage endpoint
	signal r_WB_jump_link	 	: std_ulogic;
	signal r_WB_RegWrite    	: std_ulogic;
	signal r_WB_MemtoReg    	: std_ulogic;
    signal r_WB_adresse_PC_plus_4     : std_ulogic_vector(31 downto 0);
	signal r_WB_AluResult			      : std_ulogic_vector(31 downto 0);
	signal r_WB_MemoryReadData			      : std_ulogic_vector(31 downto 0);
	signal r_WB_WriteRegDest_muxout	: std_ulogic_vector(4 downto 0);
	
	
	------------------------------------------------------------------------
    -- Nos signals
    ------------------------------------------------------------------------
	signal s_InsertBubble : std_logic;
	
	-- SIMD SIGNALS
    signal s_SIMD_CONTROLLER_RS    : std_ulogic_vector(4 downto 0);
    signal s_SIMD_CONTROLLER_RT    : std_ulogic_vector(4 downto 0);
    signal s_SIMD_CONTROLLER_RD    : std_ulogic_vector(4 downto 0);
    
    signal s_SIMD_MEM_TO_WRITE     : std_ulogic_vector(127 downto 0);
    signal s_SIMD_MEM_READ         : std_ulogic_vector(127 downto 0);
    
    signal s_SIMD_Register_Data1   : std_ulogic_vector(127 downto 0);
    signal s_SIMD_Register_Data2   : std_ulogic_vector(127 downto 0);
    
    signal s_SIMD_funct            : std_ulogic_vector(3 downto 0);
    signal s_SIMD_enableVectorALU  : std_ulogic;
    signal s_SIMD_RAM_ADDRESS      : std_ulogic_vector(15 downto 0);
    signal s_SIMD_enableRAM        : std_ulogic;
    
    alias  s_SIMD_AddressField_M_TYPE : std_ulogic_vector(15 downto 0) is s_Instruction(25 downto 10);
    
    signal s_SIMD_enableWrite_regs : std_ulogic;
    signal s_SIMD_register_writeData : std_ulogic_vector(127 downto 0);
    
    --- SIMD ALU Signals
    signal s_SIMD_ALU_funct        : std_ulogic_vector(3 downto 0);
    signal s_SIMD_ALU_Shamt        : std_ulogic_vector(4 downto 0);
    
    signal s_SIMD_ALUA_A           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUA_B           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUA_Result      : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUA_zero        : std_ulogic;
    
    signal s_SIMD_ALUB_A           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUB_B           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUB_Result      : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUB_zero        : std_ulogic;
    
    signal s_SIMD_ALUC_A           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUC_B           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUC_Result      : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUC_zero        : std_ulogic;
    
    signal s_SIMD_ALUD_A           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUD_B           : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUD_Result      : std_ulogic_vector(31 downto 0);
    signal s_SIMD_ALUD_zero        : std_ulogic;
    
    signal s_SIMD_ALU_OutTotal     : std_ulogic_vector(127 downto 0);
    
    signal s_enableMem_Write       : std_ulogic;
	
	
begin

o_PC	<= r_PC; -- permet au synthétiseur de sortir de la logique. Sinon, il enlève tout...


------------------------------------------------------------------------
-- Compteur de programme et mise �  jour de valeur
------------------------------------------------------------------------
process(clk)
begin
    if(clk'event and clk = '1') then
        if(reset = '1') then
            r_PC <= X"00400000";
        else
            r_PC <= s_PC_Suivant;
        end if;
    end if;
end process;

s_adresse_PC_plus_4	<= std_ulogic_vector(unsigned(r_PC) + 4);
s_adresse_jump		<= r_EX_adresse_PC_plus_4(31 downto 28) & r_EX_jump_field & "00";
s_adresse_branche	<= std_ulogic_vector(unsigned(s_imm_extended_shifted) + unsigned(r_MEM_adresse_PC_plus_4));

-- note, "i_jump_register" n'est pas dans les figures de COD5
s_PC_Suivant		<= s_adresse_jump when r_EX_jump = '1' else
                       r_EX_reg_data1 when r_EX_jump_register = '1' else 
					   s_adresse_branche when (r_MEM_branch = '1' and r_MEM_zero = '1') else
					   r_PC when s_InsertBubble = '1' else
					   s_adresse_PC_plus_4;
					   

------------------------------------------------------------------------
-- Compteur de programme et mise �  jour de valeur
------------------------------------------------------------------------
inst_MemInstr: MemInstructions
Port map ( 
	i_addresse => r_PC,
    o_instruction => s_Instruction
    );

-- branchement vers le décodeur d'instructions
o_instruction <= s_ID_Instruction_mux;
	
------------------------------------------------------------------------
-- Banc de Registres
------------------------------------------------------------------------
	   
-- Multiplexeur pour le registre en écriture
s_WriteRegDest_muxout <= c_Registre31 when r_EX_jump_link = '1' else 
                         r_EX_rt      when r_EX_RegDst = '0' else 
						 r_EX_rd;
						 
						 
s_ID_rs <= s_Instruction(25 downto 21) when i_isSimd = '0' 
        else s_Instruction(9 downto 5) when  i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M
        else s_Instruction(25 downto 21) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R;

s_ID_rt <= s_Instruction(20 downto 16);
--        else s_Instruction(25 downto 21) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R;
--        --else to_integer(s_Instruction(25 downto 10)) when  i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M

s_SIMD_CONTROLLER_RD <= s_Instruction(9 downto 5) WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M else
                        s_Instruction(15 downto 11) WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R
                        else "00000";
       
--inst_Registres: BancRegistres 
--port map ( 
--	clk          => clk,
--	reset        => reset,
--	i_RS1        => s_ID_rs,
--	i_RS2        => s_ID_rt,
--	i_Wr_DAT     => s_Data2Reg_muxout,
--	i_WDest      => r_WB_WriteRegDest_muxout,
--	i_WE         => r_WB_RegWrite,
--	o_RS1_DAT    => s_reg_data1,
--	o_RS2_DAT    => s_reg_data2
--	);

inst_Registres: BancRegistres 
port map ( 
	clk          => clk,
	reset        => reset,
	i_RS1        => s_ID_rs,
	i_RS2        => s_ID_rt,
	i_Wr_DAT     => s_Data2Reg_muxout,
	i_WDest      => r_WB_WriteRegDest_muxout,
	i_WE         => r_WB_RegWrite,
	o_RS1_DAT    => s_reg_data1,
	o_RS2_DAT    => s_reg_data2
	);

	
inst_registres_SIMD : registres_SIMD
PORT MAP(
    clk          => clk,
	reset        => reset,
	i_RS1        => s_SIMD_CONTROLLER_RS,
	i_RS2        => s_SIMD_CONTROLLER_RT,
	i_Wr_DAT     => s_SIMD_register_writeData, -- DATA READ FROM MEMORY CONTAINING 4 BLOCKS OF INFORMATION
	i_WDest      => s_SIMD_CONTROLLER_RD,
	i_WE         => s_SIMD_enableWrite_regs,
	o_RS1_DAT    => s_SIMD_Register_Data1,
	o_RS2_DAT    => s_SIMD_Register_Data2
);

s_SIMD_register_writeData <= s_SIMD_MEM_READ when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M AND s_Instruction(4 downto 1) = OP_LWV
                             else s_SIMD_ALU_OutTotal;

s_SIMD_CONTROLLER_RS <= s_Instruction(25 downto 21) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else "00000";
s_SIMD_CONTROLLER_RT <= s_Instruction(20 downto 16) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else
                        s_Instruction(9 downto 5)   when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M AND s_Instruction(4 downto 1) = OP_SWV else
                        "00000";
-- s_SIMD_CONTROLLER_RD <= s_Instruction(15 downto 11) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else "00000"; Already set up

-- Enable if type R , or , type M and Loading from RAM
s_SIMD_enableWrite_regs <= '1' WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else
                           '1' WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M AND s_Instruction(4 downto 1) = OP_LWV else
                           '0';

inst_ALU_VectorielleA : alu
port map(
    i_a         => s_SIMD_ALUA_A,
	i_b         => s_SIMD_ALUA_B,
	i_alu_funct => s_SIMD_ALU_funct,
	i_shamt     => s_SIMD_ALU_Shamt,
	i_isSimd    => i_isSimd,
	o_result    => s_SIMD_ALUA_Result,
	o_v         => open,
	o_zero      => s_SIMD_ALUA_zero
);

inst_ALU_VectorielleB : alu
port map(
    i_a         => s_SIMD_ALUB_A,
	i_b         => s_SIMD_ALUB_B,
	i_alu_funct => s_SIMD_ALU_funct,
	i_shamt     => s_SIMD_ALU_Shamt,
	i_isSimd    => i_isSimd,
	o_result    => s_SIMD_ALUB_Result,
	o_v         => open,
	o_zero      => s_SIMD_ALUB_zero
);

inst_ALU_VectorielleC : alu
port map(
    i_a         => s_SIMD_ALUC_A,
	i_b         => s_SIMD_ALUC_B,
	i_alu_funct => s_SIMD_ALU_funct,
	i_shamt     => s_SIMD_ALU_Shamt,
	i_isSimd    => i_isSimd,
	o_result    => s_SIMD_ALUC_Result,
	o_v         => open,
	o_zero      => s_SIMD_ALUC_zero
);

inst_ALU_VectorielleD : alu
port map(
    i_a         => s_SIMD_ALUD_A,
	i_b         => s_SIMD_ALUD_B,
	i_alu_funct => s_SIMD_ALU_funct,
	i_shamt     => s_SIMD_ALU_Shamt,
	i_isSimd    => i_isSimd,
	o_result    => s_SIMD_ALUD_Result,
	o_v         => open,
	o_zero      => s_SIMD_ALUD_zero
);
-- Pour tous les ALUS
s_SIMD_ALU_funct <= s_Instruction(3 downto 0) WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else ALU_NULL;
s_SIMD_ALU_Shamt <= s_Instruction(10 downto 6) WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_R else "00000";
s_SIMD_ALU_OutTotal <= s_SIMD_ALUA_Result & s_SIMD_ALUB_Result & s_SIMD_ALUC_Result & s_SIMD_ALUD_Result;

-- ALU A
s_SIMD_ALUA_A <= s_SIMD_Register_Data1(127 downto 96);
s_SIMD_ALUA_B <= s_SIMD_Register_Data2(127 downto 96);

-- ALU B
s_SIMD_ALUB_A <= s_SIMD_Register_Data1(95 downto 64);
s_SIMD_ALUB_B <= s_SIMD_Register_Data2(95 downto 64);

-- ALU C
s_SIMD_ALUC_A <= s_SIMD_Register_Data1(63 downto 32);
s_SIMD_ALUC_B <= s_SIMD_Register_Data2(63 downto 32);

-- ALU D
s_SIMD_ALUD_A <= s_SIMD_Register_Data1(31 downto 0);
s_SIMD_ALUD_B <= s_SIMD_Register_Data2(31 downto 0);

------------------------------------------------------------------------
-- ALU (instance, extension de signe et mux d'entrée pour les immédiats)
------------------------------------------------------------------------
-- extension de signe et décalage
s_imm_extended <= std_ulogic_vector(resize(  signed(s_ID_imm16),32)) when i_SignExtend = '1' else -- extension de signe �  32 bits
				  std_ulogic_vector(resize(signed(s_Instruction(25 downto 10)),32)) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M else
				  std_ulogic_vector(resize(unsigned(s_ID_imm16),32)); 
s_imm_extended_shifted <= r_MEM_imm_extended(29 downto 0) & "00";

-- Mux pour immédiats

s_AluB_data <=  "0000000000000000" & s_Instruction(25 downto 10) when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M else 
                r_EX_reg_data2 when r_EX_ALUSrc = '0' else
                r_EX_imm_extended;

s_AluA_data <= X"10010000" when i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M else s_reg_data1;

inst_Alu: alu 
port map( 
	i_a         => s_AluA_data,
	i_b         => s_AluB_data,
	i_alu_funct => r_EX_alu_funct,
	i_shamt     => r_EX_shamt,
	i_isSimd    => '0',
	o_result    => s_AluResult,
	o_v         => open,
	o_zero      => s_zero
	);

------------------------------------------------------------------------
-- Mémoire de données
------------------------------------------------------------------------
inst_MemDonnees : MemDonnees
Port map( 
	clk => clk,
	reset => reset,
	i_MemWrite => s_enableMem_Write,
    i_Addresse => r_MEM_AluResult,
	i_WriteData => r_MEM_reg_data2, -- POTENTIAL BUG??? , was r_EX_reg_data2
	i_enable_SIMD => i_isSimd,
	i_WriteData_SIMD => s_SIMD_Register_Data2, -- SIGNAL 128 bits sortie du banc de registres SIMD
    o_ReadData => s_MemoryReadData,
    o_ReadData_SIMD => s_SIMD_MEM_READ -- SIGNAL 128 bits
	);
	
s_enableMem_Write <= '1' WHEN i_isSimd = '1' AND i_SIMD_type = OP_SIMD_TYPE_M AND s_Instruction(4 downto 1) = OP_SWV 
                     else r_MEM_MemWrite;
	

------------------------------------------------------------------------
-- Mux d'écriture vers le banc de registres
------------------------------------------------------------------------

s_Data2Reg_muxout    <= r_WB_adresse_PC_plus_4 when r_WB_jump_link = '1' else
					    r_WB_AluResult         when r_WB_MemtoReg = '0' else 
						r_WB_MemoryReadData;
						
						
						
------------------------------------------------------------------------
-- Hazard controller
------------------------------------------------------------------------
inst_hazard_controller : hazard_controller PORT MAP(
    clk         => clk,
    i_IFID_rs   => std_logic_vector(s_rs),
    i_IFID_rt   => std_logic_vector(s_rt),
    i_IDEX_RD   => std_logic_vector(s_ID_rd),
    i_IDEX_RT   => std_logic_vector(s_ID_rt),
    i_EXMEM_RD  => std_logic_vector(r_EX_rd),
    i_EXMEM_RT  => std_logic_vector(r_EX_rt),
    i_MEMWB_RD  => std_logic_vector(r_MEM_WriteRegDest_muxout),
    i_MEMWB_RT  => std_logic_vector(r_MEM_WriteRegDest_muxout),
    i_OP_Code   => std_logic_vector(s_opcode),
    o_stall     => s_InsertBubble
);
------------------------------------------------------------------------
-- Registres de pipeline
------------------------------------------------------------------------
-- -- -- s_ID_Instruction_mux <= c_Nop when (s_InsertBubble = '1') else r_ID_Instruction;
s_ID_Instruction_mux <= r_ID_Instruction;

IF_ID_Instruction : process ( clk )
begin
    if(clk'event and clk = '1') then
		r_ID_adresse_PC_plus_4     <= s_adresse_PC_plus_4;
		
		if(reset = '1') then
			r_ID_Instruction <= c_Nop;
		--elsif(s_Flush = '1') then
			--r_ID_Instruction <= c_Nop;
		elsif(s_InsertBubble = '1') then
			r_ID_Instruction <= c_Nop;
		else
			r_ID_Instruction <= s_Instruction;
		end if;
		
    end if;
end process IF_ID_Instruction;						

ID_EX_Controles : process ( clk )
begin
    if(clk'event and clk = '1') then
		r_EX_alu_funct   	<= i_alu_funct;
		r_EX_ALUSrc      	<= i_ALUSrc;
		r_EX_RegDst      	<= i_RegDst;
		r_EX_branch      	<= i_branch;
		r_EX_MemWrite	  	<= i_MemWrite;
		r_EX_jump   	  	<= i_jump;
		r_EX_jump_register 	<= i_jump_register;
		r_EX_jump_link   	<= i_jump_link;
		r_EX_RegWrite    	<= i_RegWrite;
		r_EX_MemtoReg    	<= i_MemtoReg;
		
		r_EX_adresse_PC_plus_4  <= r_ID_adresse_PC_plus_4;
		r_EX_jump_field			<= s_ID_jump_field;
		r_EX_imm_extended		<= s_imm_extended;
		r_EX_reg_data1			<= s_reg_data1;
		r_EX_reg_data2			<= s_reg_data2;
		
		r_EX_rt				<= s_ID_rt;
		r_EX_rd				<= s_ID_rd;
		r_EX_shamt			<= s_ID_shamt;
    end if;
end process ID_EX_Controles;	

EX_MEM_Controles : process ( clk )
begin
    if(clk'event and clk = '1') then
		r_MEM_branch      	<= r_EX_branch      	;
		r_MEM_MemWrite	  	<= r_EX_MemWrite	  	;
		r_MEM_jump   	  	<= r_EX_jump   	  	;
		r_MEM_jump_register <= r_EX_jump_register ;
		r_MEM_jump_link   	<= r_EX_jump_link;
		r_MEM_RegWrite    	<= r_EX_RegWrite    	;
		r_MEM_MemtoReg    	<= r_EX_MemtoReg    	;
		r_MEM_adresse_PC_plus_4	<= r_EX_adresse_PC_plus_4;
		
		r_MEM_reg_data1				<= r_EX_reg_data1; -- for JR
		r_MEM_reg_data2				<= r_EX_reg_data2; -- for MEM Write
		r_MEM_AluResult				<= s_AluResult;
		r_MEM_zero					<= s_zero;
		r_MEM_WriteRegDest_muxout	<= s_WriteRegDest_muxout;
		r_MEM_imm_extended			<= r_EX_imm_extended;
    end if;
end process EX_MEM_Controles;

MEM_WB_Controles : process ( clk )
begin
    if(clk'event and clk = '1') then
		r_WB_jump_link	 	 <= r_MEM_jump_link;
		r_WB_RegWrite    	 <= r_MEM_RegWrite    	;
		r_WB_MemtoReg    	 <= r_MEM_MemtoReg    	;
		r_WB_adresse_PC_plus_4	<= r_MEM_adresse_PC_plus_4;
		r_WB_MemoryReadData	 <= s_MemoryReadData;
		r_WB_AluResult				<= r_MEM_AluResult;
		r_WB_WriteRegDest_muxout	<= r_MEM_WriteRegDest_muxout;
    end if;
end process MEM_WB_Controles;


------------------------------------------------------------
--- Pour visualisation seulement
------------------------------------------------------------
DecoupageDebug: block is
	signal s_IF_nop	: std_ulogic;
	signal s_ID_nop	: std_ulogic;
	signal s_EX_nop	: std_ulogic;
	signal s_MEM_nop	: std_ulogic;
	signal s_WB_nop	: std_ulogic;
	
    signal r_EX_Instruction     : std_ulogic_vector(31 downto 0);
    signal r_MEM_Instruction     : std_ulogic_vector(31 downto 0);
    signal r_WB_Instruction     : std_ulogic_vector(31 downto 0);
begin

process ( clk )
begin
    if(clk'event and clk = '1') then
		--if(s_ClearPipeline = '1') then -- �  ajuster selon la position des branch/jump/jal/JR
		--	r_EX_Instruction <= (others => '0');
		--	r_MEM_Instruction <= (others => '0');
		--	r_WB_Instruction <= (others => '0');
		--else
			r_EX_Instruction <= s_ID_Instruction_mux;
			r_MEM_Instruction <= r_EX_Instruction;
			r_WB_Instruction <= r_MEM_Instruction;
		--end if;
	end if;
end process;


s_IF_nop <=  '1' when (unsigned(s_Instruction) = 0) else '0';
s_ID_nop <=  '1' when (unsigned(s_ID_Instruction_mux) = 0) else '0';
s_EX_nop <=  '1' when (unsigned(r_EX_Instruction) = 0) else '0';
s_MEM_nop <= '1' when (unsigned(r_MEM_Instruction) = 0) else '0';
s_WB_nop <=  '1' when (unsigned(r_WB_Instruction) = 0) else '0';

o_InstructionDebug(12 downto  0)	<= s_IF_nop    &    s_Instruction(31 downto 26)        &   s_Instruction(5 downto 0);
o_InstructionDebug(25 downto 13)	<= s_ID_nop    &    s_ID_Instruction_mux(31 downto 26) &   s_ID_Instruction_mux(5 downto 0);
o_InstructionDebug(38 downto 26)	<= s_EX_nop    &    r_EX_Instruction(31 downto 26)     &   r_EX_Instruction(5 downto 0);
o_InstructionDebug(51 downto 39)	<= s_MEM_nop   &    r_MEM_Instruction(31 downto 26)    &   r_MEM_Instruction(5 downto 0);
o_InstructionDebug(64 downto 52)	<= s_WB_nop    &    r_WB_Instruction(31 downto 26)     &   r_WB_Instruction(5 downto 0);

end block;
------------------------------------------------------------




end Behavioral;