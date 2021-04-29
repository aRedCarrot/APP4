.data

var: .word 10

.text

.globl main

main:

	addi $t5,$zero,5
	addi $t6,$zero,6
	addi $t7,$zero,7
	
	add $t0,$zero,$t5
	addi $t8,$t0,3
	add $t1,$t5,$t6
	add $t2,$t0,$t1
	add $t3,$t5,$t5
	