
	.data
global_g:	.word	0

	.text	# The .text assembler directive indicates
	.globl main	# The following is the code
main:
# Prologue begins
	sw $ra, 0($sp)	# Save the return address
	sw $fp, -4($sp)	# Save the frame pointer
	sub $fp, $sp, 8	# Update the frame pointer
	sub $sp, $sp, 24	# Make space for the locals
# Prologue ends
label0:
	lw $s0, 16($sp)
	lw $s1, 0($s0)
	lw $s0, 12($sp)
	sw $s1, 0($s0)
	li.s $f10, 9.8
	li.s $f12, 1.2
	add.s $f14, $f10, $f12
	mov.s $f10, $f14
	lw $s0, 4($sp)
	s.s $f10, 0($s0)
	j label1
label1:
	lw $s0, 4($sp)
	l.s $f10, 0($s0)
	li.s $f12, 2.0
	c.eq.s $f10, $f12
	bc1f L_CondFalse_0
	li $s0, 1
	j L_CondEnd_0
L_CondFalse_0:
	li $s0, 0
L_CondEnd_0:
	move $s1, $s0
	bne $s1, $0, label2
	j label2
label2:
	# setting up activation record for called function
	li $s0, 3
	sw $s0, -4($sp)
	lw $s0, global_g
	sw $s0, 0($sp)
	sub $sp, $sp, 8
	jal f # function call
	add $sp, $sp, 8 # destroying activation record of called function
	move $s0, $v1 # using the return value of called function
	sw $s0, global_g
	j label3
label3:
	j epilogue_main

# Epilogue begins
epilogue_main:
	add $sp, $sp, 24
	lw $fp, -4($sp)
	lw $ra, 0($sp)
	jr $ra	# Jump back to the called procedure
# Epilogue ends
	.text	# The .text assembler directive indicates
	.globl f	# The following is the code
f:
# Prologue begins
	sw $ra, 0($sp)	# Save the return address
	sw $fp, -4($sp)	# Save the frame pointer
	sub $fp, $sp, 8	# Update the frame pointer
	sub $sp, $sp, 12	# Make space for the locals
# Prologue ends
label4:
	lw $s0, 4($sp)
	lw $s1, 0($s0)
	move $v1, $s1 # move return value to $v1
	j epilogue_f

# Epilogue begins
epilogue_f:
	add $sp, $sp, 12
	lw $fp, -4($sp)
	lw $ra, 0($sp)
	jr $ra	# Jump back to the called procedure
# Epilogue ends
