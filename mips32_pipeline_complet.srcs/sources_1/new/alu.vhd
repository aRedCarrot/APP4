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
use work.MIPS32_package.all;

entity alu is
Port ( 
	i_a          : in std_ulogic_vector (31 downto 0);
	i_b          : in std_ulogic_vector (31 downto 0);
	i_alu_funct  : in std_ulogic_vector (3 downto 0);
	i_shamt      : in std_ulogic_vector (4 downto 0);
	i_isSimd     : in std_ulogic;
	o_result     : out std_ulogic_vector (31 downto 0);
	o_v          : out std_ulogic;
	o_zero       : out std_ulogic
	);
end alu;

architecture comport of alu is
    
    signal decale 			: unsigned( 4 downto 0);
    signal s_check_overflow : std_ulogic_vector(2 downto 0);
    signal d_result 		: std_ulogic_vector (31 downto 0);
	
begin
    -- conversion de type
    decale <= unsigned(i_shamt);
    
    -- decodage et exécution de l'opération
    process(i_alu_funct, i_a, i_b, decale)
    begin
        case i_alu_funct is
            when ALU_AND => 
                d_result <= i_a and i_b;
            when ALU_OR => 
                d_result <= i_a or i_b;
            when ALU_ADD => 
                d_result <= std_ulogic_vector(signed(i_a) + signed(i_b));
            when ALU_SUB => 
                d_result <= std_ulogic_vector(signed(i_a) - signed(i_b));
            when ALU_SLL => 
                d_result <= std_ulogic_vector(signed(i_b) sll to_integer( decale ));  
            when ALU_SRL => 
                d_result <= std_ulogic_vector(signed(i_b) srl to_integer( decale )); 
			when ALU_SLL16 =>
				d_result <= std_ulogic_vector(signed(i_b) sll 16 ); 
            when ALU_SLT => 
                if(signed(i_a) < signed(i_b)) then
                    d_result <= X"00000001";      
                else
                    d_result <= X"00000000";
                end if;
            when ALU_SLTU => 
                if(unsigned(i_a) < unsigned(i_b)) then
                    d_result <= X"00000001";      
                else
                    d_result <= X"00000000";
                end if;
            when ALUV_MOVEIFV =>
                -- operation moveifv
                if i_isSimd = '1' then -- true moveifv
                    if i_b = X"00000001" THEN
                     d_result <= i_a;
                    ELSE
                     d_result <= X"00000000";
                    END IF;
                else -- null operation
                    d_result <= i_a and i_b;
                end if;
            when others =>
                d_result <= i_a and i_b;
         end case;
     end process;
     
     -- sorties spéciales, utiles pour certaines instructions
     o_zero <= '1' when (signed(d_result) = 0) else '0';
	 o_result <= d_result;
     o_v <= '0';  -- à compléter au besoin
            
end comport;
