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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package MIPS32_package is
    -- codes d'opération internes de l'ALU
    -- Nous définissons ces codes et on aurait pu adopter un autre encodage
    constant ALU_AND  : std_ulogic_vector( 3 downto 0 ) := "0000";
    constant ALU_OR   : std_ulogic_vector( 3 downto 0 ) := "0001";
    constant ALU_ADD  : std_ulogic_vector( 3 downto 0 ) := "0010";
    constant ALU_SLTU : std_ulogic_vector( 3 downto 0 ) := "0011";
    constant ALU_SUB  : std_ulogic_vector( 3 downto 0 ) := "0110";
    constant ALU_SLT  : std_ulogic_vector( 3 downto 0 ) := "0111";
    
    constant ALU_XOR  : std_ulogic_vector( 3 downto 0 ) := "1000";
    constant ALU_NOR  : std_ulogic_vector( 3 downto 0 ) := "1001";
    constant ALU_SLL  : std_ulogic_vector( 3 downto 0 ) := "1010";
    constant ALU_SRL  : std_ulogic_vector( 3 downto 0 ) := "1011";
    constant ALU_SRA  : std_ulogic_vector( 3 downto 0 ) := "1100";
    constant ALU_MULTU: std_ulogic_vector( 3 downto 0 ) := "1101";
    constant ALU_SLL16: std_ulogic_vector( 3 downto 0 ) := "1110";
    constant ALU_NULL : std_ulogic_vector( 3 downto 0 ) := "1111";
    
    -- codes du champ function des instructions de type R
    -- Ces codes sont définis par l'encodage des instructions MIPS
    -- voir entre autres p. 301 COD édition 5
    constant ALUF_SLL : std_ulogic_vector( 5 downto 0 ) := "000000";
    constant ALUF_SRL : std_ulogic_vector( 5 downto 0 ) := "000010";
    constant ALUF_SRA : std_ulogic_vector( 5 downto 0 ) := "000110";
    constant ALUF_JR  : std_ulogic_vector( 5 downto 0 ) := "001000";
    constant ALUF_ADD : std_ulogic_vector( 5 downto 0 ) := "100000";
    constant ALUF_ADDU: std_ulogic_vector( 5 downto 0 ) := "100001";
    constant ALUF_SUB : std_ulogic_vector( 5 downto 0 ) := "100010";
    constant ALUF_AND : std_ulogic_vector( 5 downto 0 ) := "100100";
    constant ALUF_OR  : std_ulogic_vector( 5 downto 0 ) := "100101";
    constant ALUF_XOR : std_ulogic_vector( 5 downto 0 ) := "100110";
    constant ALUF_NOR : std_ulogic_vector( 5 downto 0 ) := "100111";
    constant ALUF_SLT : std_ulogic_vector( 5 downto 0 ) := "101010";
    constant ALUF_SLTU: std_ulogic_vector( 5 downto 0 ) := "101011";
    
    -- Codes du champs function des instructions SIMD
    -- TYPE R
    constant ALUV_ADDUV: std_ulogic_vector(3 downto 0) := "0010"; -- ADDUV
    constant ALUV_SLTUV: std_ulogic_vector(3 downto 0) := "0111"; -- SLTUV
    constant ALUV_MOVEIFV: std_ulogic_vector(3 downto 0) := "1111"; -- MOVEIFV
    
    -- opcodes dans le décodage d'instructions
    constant OP_Rtype : std_ulogic_vector( 5 downto 0 ) := "000000";
    constant OP_J     : std_ulogic_vector( 5 downto 0 ) := "000010";
    constant OP_JAL   : std_ulogic_vector( 5 downto 0 ) := "000011";
    constant OP_BEQ   : std_ulogic_vector( 5 downto 0 ) := "000100";
    constant OP_ADDI  : std_ulogic_vector( 5 downto 0 ) := "001000";
    constant OP_ORI   : std_ulogic_vector( 5 downto 0 ) := "001101";
    constant OP_LUI   : std_ulogic_vector( 5 downto 0 ) := "001111";
    constant OP_LW    : std_ulogic_vector( 5 downto 0 ) := "100011";
    constant OP_SW    : std_ulogic_vector( 5 downto 0 ) := "101011";
    
    -- SIMD instructions
    -- INSTRUCTION BREAKDOWN TYPE SIMD_R
    -- [31 - 26]  [25 - 21]  [20 - 16] [15 - 11]  [10 - 4]  [3 - 0]
    --  OP CODE      RS         RT        RD        ???      FUNC
    constant OP_SIMD_TYPE_R: std_ulogic_vector( 5 downto 0 ) := "111000";
    
    --- Example ADDV
    ---  OP_R  $vec0 $vec1 $vec2   ???   ADDV
    --- 111000 00001 00010 00011 0000000 0010
    
    --  EXAMPLE SLTUV
    ---  OP_R  $vec1 $vec0 $vec3   ???   SLTUV
    --- 111000 00010 00001 00100 0000000 0111 => SLTUV $vec3 = $vec1 < $vec0
    ---  OP_R  $vec0 $vec1 $vec4   ???   SLTUV
    --- 111000 00001 00010 00101 0000000 0111 => SLTUV $vec4 = $vec0 < $vec1
    
    -- EXAMPLE MOVEIF
    ---  OP_R  $vec2 $vec3 $vec5   ???   MOVEIFV
    --- 111000 00011 00100 00110 0000000 1111 => MOV $vec2 -> $vec5 if $vec3 = 1
    
    ---  OP_R  $vec3 $vec0 $vec6   ???   MOVEIFV
    --- 111000 00100 00001 00111 0000000 1111 => MOV $vec3 -> $vec6 if $vec0 = 1
    
    -- INSTRUCTION BREAKDOWN TYPE SIMD_M -> Memory :)
    -- [31 - 26]  [25 - 10]  [9 - 5] [4 - 1] [0]
    --  OP CODE  MEM_ADDRESS   RD     func    ?
    constant OP_SIMD_TYPE_M: std_ulogic_vector( 5 downto 0 ) := "111001";
    constant OP_LWV        : std_ulogic_vector( 3 downto 0 ) := "1000";
    -- EXAMPLE OP_LWV => 111001 0000 0000 0000 0000 00001 1000 0 => LWV $vec0 0x10010000 => ??E440 0410??
                      -- 111001 0000 0000 0001 0000 00010 1000 0
    -- EXAMPLE OP_SWV => 111001 0000 0000 0010 0000 00011 1100 0 => SWV $vec2 0x10010020 => 
    constant OP_SWV        : std_ulogic_vector( 3 downto 0 ) := "1100";
    
    type RAM is array (natural range <>) of std_ulogic_vector (31 downto 0);
    subtype RAM256 is RAM (0 to 255);
    
	type RAM_SIMD is array(natural range <>) of std_ulogic_vector (127 downto 0);
	
    type op_type is (
		debug_OP_NOP,
        debug_OP_AND,
        debug_OP_OR,
        debug_OP_ADD,
		debug_OP_ADDU,
        debug_OP_SUB,
        debug_OP_SLL,
        debug_OP_SRL,
        debug_OP_SLT,
        debug_OP_SLTU,
        debug_OP_J,
        debug_OP_JAL,
        debug_OP_BEQ,
        debug_OP_ADDI,
        debug_OP_ORI,
        debug_OP_LUI,
		debug_OP_LW,
		debug_OP_SW,
		debug_OP_UNKNOWN_SIMD,
		debug_OP_SIMD_ADDUV,
		debug_OP_SIMD_SLTUV,
		debug_OP_SIMD_MOVEIFV,
		debug_OP_SIMD_SWV,
		debug_OP_SIMD_LWV,
        debug_update_package_list
    );
    function f_DisplayOp(InstructionDebug : std_ulogic_vector( 12 downto 0 )
                        ) return op_type;
						
	

end package MIPS32_package;

package body MIPS32_package is
	

function f_DisplayOp(InstructionDebug : std_ulogic_vector( 12 downto 0 )
                        ) return op_type is 
	variable CurrentOp : op_type;
	variable OperatorField  : std_ulogic_vector( 5 downto 0 );
	variable FunctField  : std_ulogic_vector( 5 downto 0 );
	variable NopField  : std_ulogic;
	variable SIMD_FunctField : std_ulogic_vector(3 downto 0);
	
begin
	
	OperatorField := InstructionDebug(11 downto 6);
	FunctField := InstructionDebug(5 downto 0);
	NopField := InstructionDebug(12);
	
	
	if(NopField = '1') then
		return debug_OP_NOP;
	end if;
	
	case OperatorField is
        when OP_Rtype =>
			case FunctField is 
				when ALUF_AND =>
					CurrentOp := debug_OP_AND;
				when ALUF_OR =>
					CurrentOp := debug_OP_OR;
				when ALUF_ADD =>
					CurrentOp := debug_OP_ADD;
				when ALUF_ADDU =>
					CurrentOp := debug_OP_ADDU;
				when ALUF_SUB =>
					CurrentOp := debug_OP_SUB;
				when ALUF_SLL =>
					CurrentOp := debug_OP_SLL;
				when ALUF_SRL =>
					CurrentOp := debug_OP_SRL;
				when ALUF_SLT =>
					CurrentOp := debug_OP_SLT;
				when ALUF_SLTU =>
					CurrentOp := debug_OP_SLTU;
				when others =>
					CurrentOp := debug_update_package_list;
			end case;
        when OP_J =>
			CurrentOp := debug_OP_J;
        when OP_JAL =>
			CurrentOp := debug_OP_JAL;
        when OP_BEQ =>
			CurrentOp := debug_OP_BEQ;
        when OP_ADDI =>
			CurrentOp := debug_OP_ADDI;
        when OP_LUI =>
			CurrentOp := debug_OP_LUI;
        when OP_ORI =>
			CurrentOp := debug_OP_ORI;
		when OP_LW =>
			CurrentOp := debug_OP_LW;
		when OP_SW =>
			CurrentOp := debug_OP_SW;
	    when OP_SIMD_TYPE_R =>
	        SIMD_FunctField := InstructionDebug(3 downto 0);
	        case SIMD_FunctField is 
	           when ALUV_ADDUV =>
	               CurrentOp := debug_OP_SIMD_ADDUV;
	           when ALUV_SLTUV => 
	               CurrentOp := debug_OP_SIMD_SLTUV;
	           when ALUV_MOVEIFV => 
	               CurrentOp := debug_OP_SIMD_MOVEIFV;
	           when others =>
	               CurrentOp := debug_OP_UNKNOWN_SIMD;
	         end case;
	    when OP_SIMD_TYPE_M =>
	       SIMD_FunctField := InstructionDebug(4 downto 1);
	       case SIMD_FunctField is 
	           when OP_LWV =>
	               CurrentOp := debug_OP_SIMD_LWV;
	           when OP_SWV =>
	               CurrentOp := debug_OP_SIMD_SWV;
	           when others =>
	               CurrentOp := debug_OP_UNKNOWN_SIMD;
	       end case;
		when others =>
			CurrentOp := debug_update_package_list;
	end case;
	
	return CurrentOp;
end function;

end package body MIPS32_package;