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
use IEEE.NUMERIC_STD.ALL;
use work.MIPS32_package.all;

entity controlleur is
Port (
    i_Op          	: in std_ulogic_vector(5 downto 0);
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
    o_Jump 			: out std_ulogic;
	o_jump_register : out std_ulogic;
	o_jump_link 	: out std_ulogic;
	o_SignExtend 	: out std_ulogic;
	
	-- simd
	o_isSimd        : out std_ulogic;
	o_SIMD_type     : out stD_ulogic_vector(5 downto 0);
	
    o_Err       	: out std_ulogic
    );
end controlleur;

architecture Behavioral of controlleur is

    signal s_R_funct_decode   : std_ulogic_vector(3 downto 0);

begin
    
    -- Commande à l'ALU pour les instructions "R"
    process(i_funct_field)
    begin
        case i_funct_field is
            when ALUF_AND => 
                s_R_funct_decode <= ALU_AND;
            when ALUF_OR => 
                s_R_funct_decode <= ALU_OR;
            when ALUF_ADD => 
                s_R_funct_decode <= ALU_ADD;
            when ALUF_SUB => 
                s_R_funct_decode <= ALU_SUB;                
            when ALUF_SLL => 
                s_R_funct_decode <= ALU_SLL;  
            when ALUF_SRL => 
                s_R_funct_decode <= ALU_SRL; 
            when ALUF_ADDU => 
                s_R_funct_decode <= ALU_ADD;
            when ALUF_SLT => 
                s_R_funct_decode <= ALU_SLT; 
            when ALUF_SLTU => 
                s_R_funct_decode <= ALU_SLTU; 
            -- à compléter au besoin avec d'autres instructions
            when others =>
                s_R_funct_decode <= ALU_NULL;
         end case;
     end process;

    -- Contrôles pour les différents types d'instructions
    -- Actuellement, seulement les instructions "R"
    process( i_Op, s_R_funct_decode )
    begin
		o_Err <= '0';
        case i_Op is
            when OP_Rtype => 
                o_AluFunct <= s_R_funct_decode;
			when OP_ADDI => 
				o_AluFunct <= ALU_ADD;
			when OP_ORI => 
				o_AluFunct <= ALU_OR;
			when OP_LUI => 
				o_AluFunct <= ALU_SLL16;
			when OP_BEQ => 
				o_AluFunct <= ALU_SUB;
			when OP_JAL =>
				o_AluFunct <= ALU_NULL;
			when OP_SW => 
				o_AluFunct <= ALU_ADD;
			when OP_LW => 
				o_AluFunct <= ALU_ADD;
            when others =>
                o_Err <= '1';
				o_AluFunct <= (others => '0');
        end case;
    end process; 
    
    -- simd
	o_isSimd        <= '1' when i_Op = OP_SIMD_TYPE_R
	                       else '1' when i_Op = OP_SIMD_TYPE_M 
	                       else '0';
	
    o_SIMD_type    <= i_Op; -- Field will only be used if isSimd = '1' so no need to check anything
	
	
	o_RegWrite		<= '1' when i_Op = OP_Rtype or 
								i_Op = OP_ADDI or 
								i_Op = OP_ORI or 
								i_Op = OP_LUI or 
								i_Op = OP_LW or 
								i_Op = OP_JAL
						else '0';
	
	o_RegDst 		<= '1' when i_Op = OP_Rtype else '0';
	o_Branch 		<= '1' when i_Op = OP_BEQ   else '0';
	
	o_ALUSrc 		<= '0' when i_Op = OP_Rtype or
								i_Op = OP_BEQ
						else '1';
						
	o_MemRead 		<= '0';
	o_MemWrite 		<= '1' when i_Op = OP_SW else '0';
	o_MemtoReg 		<= '1' when i_Op = OP_LW else '0';
	o_SignExtend	<= '1' when i_OP = OP_ADDI else '0';
	
	
	o_Jump	 		<= '1' when i_Op = OP_J or 
	                            i_Op = OP_JAL 
						else '0';
				
				
	o_jump_link 	<= '1' when i_Op = OP_JAL else '0';
	o_jump_register <= '1' when i_Op = OP_Rtype and 
								i_funct_field = ALUF_JR 
						else '0';

end Behavioral;
