; ABI: https://gist.github.com/2313564
; clobber a, b, c
; args: a, b, c, stack pushed left-to-right (caller cleans up)
; ret: a

:start
	set [reg_sp], 0xF000 ; separate stack
	set [reg_pc], program
:emu_loop
	; take apart instruction
	set z, [reg_pc]
	set z, [z]
	add [reg_pc], 1 ; increment here to make sure it's correct for the argument decoding
	set j, z ;j=b arg
	shr j, 10
	set i, z ;i=a arg
	shr i, 4
	and i, 0x3f
	and z, 0xf ;op
	; decode part 1
	ife z, 0
		set PC, nonbasic_op
	; read a, remembering whether it is writable in x
	ifg i, 0x1f
		set PC, a_lit
	set a, i
	jsr read_arg
	set i, a
	set x, 1 ;writable
	set PC, a_read_done
:a_lit
	sub i, 0x20
	set x, 0 ;not writable
:a_read_done
	; read b's value (never written to)
	ifg j, 0x1f
		set PC, b_lit
	set a, j
	jsr read_arg
	set j, [a]
	set PC, b_read_done
:b_lit
	sub j, 0x20
:b_read_done
	; decode op
	ifg z, 0xb
		set PC, compare_op
:arithmetic_op
	sub z, 1 ; TODO: use arithmetic_op_jump_table - 8
	shl z, 3 ; 8 words per table entry
	add z, arithmetic_op_jump_table
	set PC, z
:arithmetic_op_jump_table
:SET
	set y, j ;1
	set PC, arithmetic_store_result ;2
	dat 0, 0, 0, 0, 0 ;pad 5
:ADD
	set y, [i] ;1
	add y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:SUB
	set y, [i] ;1
	sub y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:MUL
	set y, [i] ;1
	mul y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:DIV
	set y, [i] ;1
	div y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:MOD
	set y, [i] ;1
	mod y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:SHL
	set y, [i] ;1
	shl y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:SHR
	set y, [i] ;1
	shr y, j ;1
	set [reg_o], O ;2
	set PC, arithmetic_store_result ;2
	dat 0, 0 ;pad 2
:AND
	set y, [i] ;1
	and y, j ;1
	set PC, arithmetic_store_result ;2
	dat 0, 0, 0, 0 ;pad 4
:BOR
	set y, [i] ;1
	bor y, j ;1
	set PC, arithmetic_store_result ;2
	dat 0, 0, 0, 0 ;pad 4
:XOR
	set y, [i] ;1
	xor y, j ;1
	set PC, arithmetic_store_result ;2
	; last entry -- don't pad

:arithmetic_store_result
	ife x, 1
		set [i], y
	set PC, emu_loop

:compare_op
	; get arg a's actual value
	ife x, 1
		set i, [i]
	sub z, 0xc
	shl z, 3 ;8 words per jump table entry
	add z, compare_op_jump_table
	set PC, z
:compare_op_jump_table
	; TODO: use relative jumps to cut this down to 4 words per entry
	; also, is this jump table really worth it?
	; (I think it only saves like 2 cycles on average)
:IFE
	ife i, j ;1
		set PC, emu_loop ;2
	set PC, skip_op ;2
	dat 0, 0, 0 ;pad 3
:IFN
	ifn i, j ;1
		set PC, emu_loop ;2
	set PC, skip_op ;2
	dat 0, 0, 0 ;pad 3
:IFG
	ifg i, j ;1
		set PC, emu_loop ;2
	set PC, skip_op ;2
	dat 0, 0, 0 ;pad 3
:IFB
	ifb i, j ;1
		set PC, emu_loop ;2
	set PC, skip_op ;2
	; last entry -- don't pad

:skip_flags
	dat 0xc0ff ; bitmap to check which arg values take another word

:skip_op
	; take apart instruction
	set z, [reg_pc]
	set z, [z]
	add [reg_pc], 1
	set j, z ;j=b arg
	shr j, 10
	set i, z ;i=a arg
	shr i, 4
	and i, 0x3f
	and z, 0xf ;op
:skip_arg_b
	sub j, 0x10
	ifg j, 0x10
		set PC, skip_arg_a
	set a, 1
	shl a, j
	ifb a, [skip_flags]
		add [reg_pc], 1
	; skip other arg for nonbasics
	ife z, 0
		set PC, emu_loop
:skip_arg_a
	sub i, 0x10
	ifg i, 0x10
		set PC, emu_loop
	set a, 1
	shl a, i
	ifb a, [skip_flags]
		add [reg_pc], 1
	set PC, emu_loop

:nonbasic_op
	; decode
	ifn i, 1
		set PC, err	
	; read a (in b's spot)
	ifg j, 0x1f
		set PC, nb_a_lit
	set a, j
	jsr read_arg
	set j, [a]
	set PC, nb_a_read_done
:nb_a_lit
	sub j, 0x20
:nb_a_read_done
	;JSR
	sub [reg_sp], 1
	set a, [reg_sp]
	set [a], [reg_pc]
	set [reg_pc], j	
	set PC, emu_loop

		

:err
; TODO: print an error
	sub PC, 1

; Takes arg code in a & returns address of value in a
; May adjust reg_pc
; Only for non-literals.
:read_arg
	; register?
	ifg 0x08, a
		set PC, read_reg
	ifg 0x10, a
		set PC, deref_reg
	ifg 0x18, a
		set PC, deref_nextword_reg
	; use jump table
	sub a, 0x18
	shl a, 3 ; 8 words per jump table entry
	add a, read_arg_jump_table
	set PC, a

:read_reg
	add a, registers
	set PC, pop
:deref_reg
	sub a, 0x08
	add a, registers
	set a, [a]
	set PC, pop
:deref_nextword_reg
	sub a, 0x10
	add a, registers
	set a, [a]
	set b, [reg_pc]
	add a, [b]
	add [reg_pc], 1
	set PC, POP

	; TODO: extend assembler to support @read_arg_jump_table+0x14 directives
	; also, check the padding
:read_arg_jump_table
	; TODO: like half of this is padding. compactify?
:rajt_pop
	set a, [reg_sp] ;2
	add [reg_sp], 1 ;2
	set PC, pop ;1
	dat 0, 0, 0 ;pad 3
:rajt_peek
	set a, [reg_sp] ;2
	set PC, POP ;1
	dat 0, 0, 0, 0, 0 ;pad 5
:rajt_push
	sub [reg_sp], 1 ;2
	set a, [reg_sp] ;2
	set PC, POP ;1
	dat 0, 0, 0 ;pad 3
:rajt_sp
	set a, reg_sp ;2
	set PC, POP ;1
	dat 0, 0, 0, 0, 0 ;pad 5
:rajt_pc
	set a, reg_pc ;2
	set PC, POP ;1
	dat 0, 0, 0, 0, 0 ;pad 5
:rajt_o
	set a, reg_o
	set PC, POP
	dat 0, 0, 0, 0, 0 ;pad 5
:rajt_deref_nextword
	set a, [reg_pc] ;2
	set a, [a] ;1
	add [reg_pc], 1 ;2
	set PC, POP ;1
	dat 0, 0 ;pad 2
:rajt_nextword
	set a, [reg_pc] ;2
	add [reg_pc], 1 ;2
	set PC, POP ;1
	;last one -- don't pad

:registers
:reg_a
	dat 0
:reg_b
	dat 0
:reg_c
	dat 0
:reg_x
	dat 0
:reg_y
	dat 0
:reg_z
	dat 0
:reg_i
	dat 0
:reg_j
	dat 0
:reg_pc
	dat 0
:reg_sp
	dat 0
:reg_o
	dat 0

:program
; Example from http://0x10c.com/doc/dcpu-16.txt
; Try some basic stuff
                      SET A, 0x30              ; 7c01 0030
                      SET [0x1000], 0x20       ; 7de1 1000 0020
                      SUB A, [0x1000]          ; 7803 1000
                      IFN A, 0x10              ; c00d 
                         SET PC, crash         ; 7dc1 001a [*]
                      
        ; Do a loopy thing
                      SET I, 10                ; a861
                      SET A, 0x2000            ; 7c01 2000
        :loop         SET [0x2000+I], [A]      ; 2161 2000
                      SUB I, 1                 ; 8463
                      IFN I, 0                 ; 806d
                         SET PC, loop          ; 7dc1 000d [*]
        
        ; Call a subroutine
                      SET X, 0x4               ; 9031
                      JSR testsub              ; 7c10 0018 [*]
                      SET PC crash             ; 7dc1 001a [*]
        
        :testsub      SHL X, 4                 ; 9037
                      SET PC, POP              ; 61c1
                        
        ; Hang forever. X should now be 0x40 if everything went right.
        :crash        SET PC, crash            ; 7dc1 001a [*]
        
        ; [*]: Note that these can be one word shorter and one cycle faster by using the short form (0x00-0x1f) of literals,
        ;      but my assembler doesn't support short form labels yet.     
