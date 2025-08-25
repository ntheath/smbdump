;------------------------------------------------------------------------
;smbdump - SMBIOS dump utility
;
;Author:  Nicholas Heath
;
;Copyright 2003, Dell Computer Corp.
;
;Version: 1.0
;------------------------------------------------------------------------
.386p					;use 32 bit regs

include equates.inc
include smbios.inc

;flat_data	equ 	8		;used for real flat mode

STACK_SEG segment para use16 stack 'STACK'

	db	256 dup(' ')
	;TOP_OF_STACK DW ?			;not needed if 'STACK' is used in stack
						;segment declaration

STACK_SEG ends

DATA_SEG segment para use16 public 'DATA'

;GDT		dw	0Fh, GDT, 0, 0, 0FFFFh, 0, 9200h, 8Fh
hello_message 	db 	'smbdump, SMBIOS Table dump utility, v1.0',CR,LINE_FEED,'$'
hello_message_2 db 	'Copyright Nick Heath, ', 60h, '03, Dell Computer Corp.',CR,LINE_FEED,'$'
new_line 	db 	CR, LINE_FEED,'$'

No_anchor	db	'SMBIOS table anchor string not found!', '$'
Success_msg	db	'Anchor string located!','$'
Anchor_found	db	'SMBIOS anchor string, _SM_, found at:  ','$'
Anchor_seg	db	'Segment:  ','$'
Anchor_offset	db	'Offset:  ', '$'
Entry_check_msg	db	'Entry point structure checksum:  ','$'
Entry_length_msg	db	'Entry point length:  ','$'
SMBIOS_major_msg	db	'SMBIOS Major Version:  ', '$'
SMBIOS_minor_msg	db	'SMBIOS Minor Version:  ', '$'
Max_struct_msg	db	'Max structure size:  ','$'
Inter_anchor_msg	db	'Intermediate anchor string:  ','$'
Inter_check_msg	db	'Intermediate checksum:  ','$'
Struct_table_msg	db	'SMBIOS Table length:  ','$'
Struct_add_msg	db	'SMBIOS Table address:  ','$'
Num_strucs_msg	db	'Number of structures in table:  ','$'
BCD_rev_msg	db	'SMBIOS BCD rev:  ','$'
Calc_table_seg	db	'SMBIOS table at segment:  ','$'
Calc_table_off	db	'Offset:  ', '$'
Type_msg	db	'Type:  ','$'
Len_msg		db	'Length:  ','$'
Handle_msg	db	'Handle:  ','$'
Struct_msg	db	'Structure found at:  ','$'
debug_msg	db	'GETTING HERE!','$'

;print_ASCIItoHEX lookup tables
ASCIIAtoF 	db 	41h, 42h, 43h, 44h, 45h, 46h	
ASCII0to9 	db 	30h, 31h, 32h, 33h, 34h, 35h, 36h, 37h, 38h, 39h

EPS_offset	dw	?		;16bit offset of EPS
temp_space	db	'DEBUG!','$'
Table32		dd	?		;32bit address of SMBIOS table
Table_offset	dw	?		;16bit offset of SMBIOS table
Table_segment	dw	?		;16bit segment of SMBIOS table
Num_structs	dw	?		;16bit value for # of SMBIOS structs
Table_len	dw	?		;16bit value for length of table in bytes

Data_buff	db	256 dup("*")	;data buffer for each structure, 256 bytes

Data_bytes_read	dw	0		;data buffer offset, used for indexing
Bytes_displayed	db	0		;used to format 16 bytes per line in output to screen

;Structure lookup table
Structure_type	db	'BIOS Information','$'
Structure_table	dw	0h,0h,0h,'$'

;, '$','System Information', 'Base Board Information','$'


DATA_SEG ends

CODE_SEG segment para use16 public 'CODE'
	assume cs:CODE_SEG, ds:DATA_SEG, es:nothing, ss:STACK_SEG

	org 	$+2
					
main proc near
	mov	ax, DATA_SEG		;setup data segment
	mov	ds, ax
	;mov	ax, STACK_SEG		;setup stack segment
    	;mov	ss, ax
    	;mov	sp, offset TOP_OF_STACK	;setup stack pointer, not needed if 'STACK' is used in
	;nop				;the declaration of the stack segment

	call	print_info		;print program information
	call 	locate_anchor		;first need to find the SMBIOS table
					;anchor string in memory
	
				
	mov	ds:[EPS_offset], ax	;load value returned by locate_anchor into 
					;memory
					;ds:[EPS_offset] now contains address of EPS
	
	call 	parse_entry_structure	;take EPS apart and display info
					
	mov	ds:[Table32], edx	;edx,Table32 contain the 32bit address of the SMBIOS table
	
	mov	eax, edx		;load address into eax before calling calculate_...
	
	call	calculate_table_address ;convert 32bit table address to 
					;segment:offset form
	mov	ds:[Table_offset], dx	;put our freshly converted addresses into memory
	mov	ds:[Table_segment], bx	;...			
					
	;The table_len and num_structs has been loaded into memory
	;the parse_table proc will use these values to parse through
	;the SMBIOS table, decrementing Num_structs with each structure
	;that is parsed, and checking the Table_len to see if it matches
	;the length of the table in memory.
	
	xor	ecx, ecx
	mov	cx, ds:[Num_structs]	;load # of structs in table to parse/print	
main_loop:	
	call	parse_structure		;parse a struct into data buffer
	call	print_structure		;print struct in data buffer
loop	main_loop
			
		
	mov 	ax,4C00h			;terminate process
      	int 	21h								
main endp

;------------------------------------------------------------------------
;Procedure:  locate_anchor
;
;Function:  Locates the SMBIOS anchor string, _SM_  (5F 53 4D 5F) in 
;	    memory range 0xF0000 - 0xFFFFF.  Uses es = F000h, bx = 0000h 
;
;On entry:  none
;
;Returns: memory location of anchor string in memory, address stored in
;	  ax
;Calls:  print_ASCIItoHex, print_new_line
;------------------------------------------------------------------------
locate_anchor proc
	;pusha				;backup regs
	push	bx
	push	cx
	push	dx
	
	mov 	ax, LOW_LIMIT		;load es (or fs, gs) with segment
					;we need to search, in this case
					;we know the anchor string is located
					;somewhere in F0000h - FFFFFh in memory
					;F0000h - offset / 16 = F000h.
	mov	es, ax
	
	xor	ax, ax			;clean up regs
	mov 	bx, ax			;setup base register for es:bx

search_5F:
	mov 	al, es:[bx]		;get first byte
	mov	ah, 5fh			;first hex character to look for

	cmp	al, ah			;see if we found the character
	je	found_			;if so, proceed to the next char
	
	cmp 	bx, HIGH_LIMIT		;make sure we haven't gone too far
	je	not_found		;character not found in memory
					;range
	
	add	bx, 10h			;we haven't reached FFFF or found 
	and	bx, 0FFF0h		;char, so increment to next
					;paragraph and start over
	jmp	search_5F
found_:
	add	bx, 1h			;increment for next search byte
	mov 	ah, 53h			;check the second char, 53h
	mov 	al, es:[bx]
	cmp 	al, ah	
	je	foundS			;proceed to next char if we find 'S'
	add	bx, 0Fh
	jmp 	search_5F		;if not, go back to looking for '5F'
	
foundS:
	add	bx, 1h			;increment for next search byte
	mov 	ah, 4Dh			;check the second char, 4Dh
	mov 	al, es:[bx]
	cmp 	al, ah	
	je	foundM
	add	bx, 0Eh
	jmp 	search_5F

foundM:

	add	bx, 1h			;increment for next search byte
	mov 	ah, 5Fh			;check the second char, 53h
	mov 	al, es:[bx]
	cmp 	al, ah	
	je	success_finished
	add	bx, 0Dh
	jmp 	search_5F

not_found:
		
	mov 	ah, 09h
      	mov 	dx,offset No_anchor
      	int 	21h
      	mov	bx, 0FFFFh 
	jmp 	finished

success_finished:
	sub	bx, 3			;currently at end of anchor, sub 3
					;to get to the starting address
	mov	ah, 09h
	lea	dx, Anchor_found	;print success message
	;int 	21h
	call	print_new_line
		
	lea	dx, Anchor_seg		;print segment where we found anchor
	mov	ah, 09h
	int 	21h
	mov	dx, es			;get the segment
	mov	al, dh			;and print each byte(2)
	call	print_ASCIItoHEX
	mov	al, dl
	call	print_ASCIItoHEX
	
	call 	print_SPACE		;formatting
	call	print_SPACE
	
	lea	dx, Anchor_offset	;now print offset info
	mov	ah, 09h
	int	21h
	mov 	al, bh			
	call 	print_ASCIItoHex
	mov 	al, bl
	call 	print_ASCIItoHex
		
	call 	print_new_line		;formatting
	
finished:	
	;mov	ds:[EPS_offset], bx	;save offset into memory for use
					;by other procs
	mov	ax, bx			;return address in ax
	pop	dx
	pop	cx
	pop	bx
	
	;popa				;restore all regs used
	ret
locate_anchor endp

;------------------------------------------------------------------------
;Procedure:  parse_entry_structure
;
;Function:  Parses the SMBIOS entry structure (EPS)
;
;On entry:  Uses EPS_offset stored in ax to locate the entry structure
;
;Returns: Table32 in edx contains 32bit address of SMBIOS table
;         
;Calls:  print_SPACE, print_new_line, print_ASCIItoHEX
;------------------------------------------------------------------------
parse_entry_structure proc
	;pusha				;backup regs
	
	push	ax
	push	bx
	push	cx
	
	mov	bx, ax			;load EPS_offset
	mov	ax, LOW_LIMIT		;load segment of EPS
	mov	es, ax
	
	;xor	bx, bx
	;mov	bx, ds:[EPS_offset]	;load offset of EPS
		
	;start parsing the entry structure, starting with checksum
	mov	ah, 09h			;print status message
	lea	dx, Entry_check_msg
	int	21h
	
	mov	al, es:[bx+ENTRY_CHECKSUM_OFFSET]		
					;get our byte
	call 	print_ASCIItoHEX	;print out our checksum

	;finished with the checksum, now get entry point length
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Entry_length_msg
	int	21h
		
	mov	al, es:[bx+ENTRY_LENGTH_OFFSET]	
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	;move on to SMBIOS Major and Minor version bytes
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, SMBIOS_major_msg
	int	21h
	
	mov	al, es:[bx+MAJOR_VER_OFFSET]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, SMBIOS_minor_msg
	int	21h
	
	mov	al, es:[bx+MINOR_VER_OFFSET]
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	;move on to Max structure size (word)
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Max_struct_msg
	int	21h

	mov	al, es:[bx+MAX_STRUCT_SIZE_OFFSET+1]		
					;get our byte, +1 for first byte
	call	print_ASCIItoHEX	;print our byte
	
	mov	al, es:[bx+MAX_STRUCT_SIZE_OFFSET]
	call	print_ASCIItoHEX
		
	;move on to Intermediate anchor string (word + 1 byte)
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Inter_anchor_msg
	int	21h

	mov	al, es:[bx+INTER_ANCHOR_OFFSET]		
					;get our byte, '_'
	call	print_ASCIItoHEX	;print our byte
	call	print_SPACE
		
	mov	al, es:[bx+INTER_ANCHOR_OFFSET+1]		
					;get our byte, 'D'
	call	print_ASCIItoHEX	;print our byte
	
	call 	print_SPACE		;make it readable
	
	mov	al, es:[bx+INTER_ANCHOR_OFFSET+2]		
					;get our byte, 'M'
	call	print_ASCIItoHEX	;print our byte
	
	call 	print_SPACE		;make it readable
	
	mov	al, es:[bx+INTER_ANCHOR_OFFSET+3]		
					;get our byte, 'I'
	call	print_ASCIItoHEX	;print our byte
	
	call 	print_SPACE		;make it readable
	
	mov	al, es:[bx+INTER_ANCHOR_OFFSET+4]		
					;get our byte, '_'
	call	print_ASCIItoHEX	;print our byte
	
	;move on to Intermediate checksum
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Inter_check_msg
	int	21h

	mov	al, es:[bx+INTER_CHECKSUM_OFFSET]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	;move on to structure table length (word)
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Struct_table_msg
	int	21h

	mov	al, es:[bx+TABLE_LENGTH_OFFSET+1]		
					;get our byte, +1 for first byte
	call	print_ASCIItoHEX	;print our byte
	
	mov	al, es:[bx+TABLE_LENGTH_OFFSET]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte	
	
	;grab the table length and put it into memory
	;for future procs to use, in Table_len
	
	;copy uses movsw = Move string word
	;moves word @ DS:SI to ES:DI so since we're backwards
	;with this snytax, we need to reverse our ds and es
	;segments.  Currently, es = segment where the byte we 
	;want to copy is located, and ds is the destination
	;in memory where we want to put the byte.
	push	es			;back up regs
	push	ds			;...
	push	ax			;...
	push 	dx			;...
	
	mov	ax, es			;prepare to move es into ds
	mov	dx, ds			;prepare to move ds into es
	mov	ds, ax			;ds = es
	mov	es, dx			;es = ds
	

	lea	di, Table_len		;prime si with the destination offset
					;where Table_len is located
	lea	si, [bx+TABLE_LENGTH_OFFSET]
					;prime si with the address in memory 
					;where we're copying from
	movsw				;copy one word from DS:SI to ES:DI
	
	pop	dx			;restore all regs used
	pop	ax			;...
	pop	ds			;...
	pop	es			;...
		
	
	;move on to structure table address (dword)
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Struct_add_msg
	int	21h		
	
	mov	ecx, es:[bx+TABLE_ADDRESS_OFFSET]	
					;load 4 bytes of address
					;into ecx
	;mov	ds:[Table32], ecx	;put our 32bit address in memory for other
					;procs to use @ Table32
	
	mov	edx, ecx		;we're going to display the address now,
					;backup our address because we need to shift
					;the reg down 16bits to display them in the 
					;correct order
	push	edx			;backup 32bit address
	
	shr	ecx, 16			;shift
	mov	al, ch			;display high byte
	call	print_ASCIItoHEX
	mov	al, cl			;display low byte
	call	print_ASCIItoHEX
	
	mov	cx, dx			;restore lower 16 bits of ecx
	mov	al, ch			;display high byte
	call	print_ASCIItoHEX
	mov	al, cl			;display high byte
	call	print_ASCIItoHEX
	
	;move on to number of SMBIOS structures (word)
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, Num_strucs_msg
	int	21h

	mov	al, es:[bx+NUM_STRUCTS_OFFSET+1]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	mov	al, es:[bx+NUM_STRUCTS_OFFSET]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	
	;grab the number of structs and put it into memory
	;for future procs to use
	
	;copy uses movsw = Move string word
	;moves word @ DS:SI to ES:DI so since we're backwards
	;with this snytax, we need to reverse our ds and es
	;segments.  Currently, es = segment where the byte we 
	;want to copy is located, and ds is the destination
	;in memory where we want to put the byte.
	;mov	ds:[Num_structs], word ptr es:[bx+NUM_STRUCTS_OFFSET]
	
	push	es			;back up regs
	push	ds			;...
	push	ax			;...
	push 	dx			;...
	
	mov	ax, es			;prepare to move es into ds
	mov	dx, ds			;prepare to move ds into es
	mov	ds, ax			;ds = es
	mov	es, dx			;es = ds
	

	lea	di, Num_structs		;prime si with the destination offset
					;where Num_structs is located
	lea	si, [bx+NUM_STRUCTS_OFFSET]
					;prime si with the address in memory 
					;where we're copying from
	movsw				;copy one word from DS:SI to ES:DI
	
	pop	dx			;restore all regs used
	pop	ax			;...
	pop	ds			;...
	pop	es			;...
	
	
	;move on to the final field, SMBIOS BCD rev
	call 	print_new_line
	mov	ah, 09h			;print status message
	lea	dx, BCD_rev_msg
	int	21h
		
	mov	al, es:[bx+BCD_REV_OFFSET]		
					;get our byte
	call	print_ASCIItoHEX	;print our byte
	
	pop	edx			;restore our 32bit address
	pop	cx
	pop	bx
	pop	ax
	;popa				;restore regs
	ret
parse_entry_structure endp

;------------------------------------------------------------------------
;Procedure:  calculate_table_address
;
;Function:  Calculates the table offset, segment from the 32bit address
;
;On entry:  eax contains 32bit address SMBIOS table
;
;Returns:  dx contains Table_offset - the offset generated from the 32bit address
;	   bx contains Table_segment - the segment generated from the 32bit address
;         
;Calls:  
;------------------------------------------------------------------------
calculate_table_address proc
	;eax - Table32
	push	ax
	push	cx
	
	;Table32 contains 32bit address, we need to subtract away the offset
	;and divide by 10h to get the segment, then load these two values
	;into Table_offset and Table_segment respectively
	mov	eax, ds:[Table32]	;get our 32bit table address

	mov	dx, ax			;just move lower 16bits into offset reg
	sub	ax, dx			;subtract away the offset from 32bit address
	shr	eax, 4			;divide by 10h, ax now contains segment
	mov	bx, ax			;return segment
	
	pop	cx			;restore regs
	pop	ax
	ret
calculate_table_address endp

;------------------------------------------------------------------------
;Procedure:  parse_structure
;
;Function:  Parses a SMBIOS structure
;
;On entry:  The SMBIOS table offset and segment are stored in memory
;	    at Table_offset and Table_segment respectively.
;    	  
;
;Returns: Table_offset will be updated with the current offset for the next
;	  structure; structure data is put into data_buff in memory.
;         
;Calls:	
;------------------------------------------------------------------------
parse_structure proc
	pusha				;back up regs
		
	call	print_new_line		;formatting...
		
	mov	ax, ds:[Table_segment]	;prepare es for the SMBIOS table segment
	mov	es, ax			;...
	
	mov	si, ds:[Table_offset]	;load address of source
	lea	di, Data_buff		;load address of destination (data buffer)
	
	mov	bx, ds:[Table_offset]	;prime bx for indexing into memory, used as offset in next line
	xor	cx, cx			;make sure cx is clean before using as a counter reg
	mov	cl, byte ptr es:[bx+1]	;load counter (number of bytes to read w/o strings)	
	add	cx, 2h			;read two extra bytes (00h 00h) at the end of the struct
					;these will be checked to determine the presence of strings
	
	xor	bx, bx			;make sure to clean out bx, it will store number of bytes read into
					;data buffer
	;need to swap segments, es and ds due to syntax and implementation differences
	;normally:  es = destination, ds = source
	;my implemenation:  ds = destination, es = source
			
	push	es			;backup regs before swapping
	push	ds			;...
	mov	ax, ds:[Table_segment]
	mov	dx, ds			;prepare to swap es/ds
	mov	ds, ax			;ds = es
	mov	es, dx			;es = ds
	
read_byte:
	movsb				;move one byte of data into the buffer
	inc	bx			;increment number of bytes read
	loop read_byte			;loop until our counter is = 0 = length of struct + 2 bytes

read_dyn_bytes:				;need to take care of the variable
					;length strings at the end of the 
					;structures
	;check last two bytes read
	dec	di			;check the last byte read
	mov 	al, byte ptr es:[di]	
	cmp 	al, 00h			;if its 00h, then need to check
	je	check_next_last_byte	;next to last byte too
	inc 	di			;if not, keep reading
	movsb				
	inc	bx			;increment number of bytes read
	jmp	read_dyn_bytes		;check the byte we just read
check_next_last_byte:
	dec	di			;we have one 00h, lets check the
	mov	al, byte ptr es:[di]	;next to last byte
	cmp	al, 00h			
	je 	done			;if we have 00h, we're done
	inc	di			;if not, go back to the end 
	inc	di			;and move a byte
	movsb
	inc	bx			;increment number of bytes read	
	jmp	read_dyn_bytes		;check the byte we just read
done:
	mov	es:[Table_offset], si	;update the source offset in memory
	mov	es:[Data_bytes_read], bx ;update number of bytes read into our buffer
	
	pop	ds			;restore (unswap) segs
	pop	es			;...
	popa				;restore regs
	ret
parse_structure endp

;------------------------------------------------------------------------
;Procedure:  print_structure
;
;Function:  Prints a SMBIOS structure
;
;On entry:  The current structure in the data buffer is printed out
;	    
;    	  
;
;Returns: prints type, len, and handle as well as the raw data in bytes
;	  of the SMBIOS structure in the data buffer
;         
;Calls:	
;------------------------------------------------------------------------
print_structure proc
	pusha
				
	call	print_new_line		;formatting
	call	print_new_line		;...
	mov	ds:[Bytes_displayed], 0	;reset the bytes displayed per line variable
	lea	dx, Struct_msg		;print message structure found msg
	mov	ah, 09h
	int 	21h
		
	mov	bx, ds:[Table_offset]	;get current table offset
	mov	cx, ds:[Data_bytes_read];get current data offset
	sub	bx, cx			;subtract the # bytes read from table offset
					;to display correct location of structure in memory
	mov	al, bh			;prepare to high print byte
	call	print_ASCIItoHEX	;print byte in hex
	mov	al, bl			;print low byte
	call	print_ASCIItoHEX
		
	call	print_new_line
	mov	ah, 09h			;print type message
	lea	dx, Type_msg
	int 	21h
		
	mov	al,byte ptr ds:[Data_buff]
	call	print_ASCIItoHEX
	
	call	print_new_line
	lea	dx, Len_msg		;print len message
	int	21h
	
	mov	al, byte ptr ds:[Data_buff+1]
	call	print_ASCIItoHEX
	
	call	print_new_line
	lea	dx, Handle_msg		;print handle message (word)
	int	21h
	
	mov	bx,word ptr ds:[Data_buff+2]
	mov	al, bh
	call	print_ASCIItoHEX	;print second byte of handle
	mov	al, bl
	call	print_ASCIItoHEX

	sub	cx, 4h			;need to sub 4 due to last two bytes read
					;in from parse_structure, as well as the 3
					;bytes just read in (type, len, handle).  Want
					;to start at offset+4 bytes			
	call	print_new_line		;formatting
		
	xor	dx, dx			;clean up dx				
	mov	bx, 0004h		;start printing at byte location 4
print_bytes:
	mov	dl, byte ptr ds:[Data_buff+bx]	;get one byte from the data buffer
	inc 	bx				;increment # of bytes read
	mov	al, ds:[Bytes_displayed]	;increment # of bytes displayed
	inc	ax				;...
	mov	ds:[Bytes_displayed], al	;...
	mov	al, dl				;prepare to print byte to std. out
	cmp	ds:[Bytes_displayed], BYTES_PER_LINE	;check if we need newline
	jne	fill_up_line			;if not, keep outputting
	call	print_new_line			;if so, print new line
	mov	ds:[Bytes_displayed], 1		;reset line count
fill_up_line:	
	call	print_ASCIItoHEX		;print byte to output
	call	print_SPACE			;don't forget space
	loop 	print_bytes			;start over
	
	popa
	ret
print_structure endp

;------------------------------------------------------------------------
;Procedure:  print_info
;
;Function:  Prints program info
;
;On entry:  none
;
;Returns: info to stdout
;
;Calls:  print_new_line
;------------------------------------------------------------------------
print_info proc
  	push 	ax			;backup regs
  	push 	dx			;...
        xor	ax, ax			;clean up regs
  	mov 	ah,9			;prepare for string output
  	;mov 	dx,offset hello_message	;load add of string into dx
  	lea 	dx, hello_message	;load effective address of hello_message
  	int 	21h			;print string 		
 	lea	dx, hello_message_2
	int	21h
	call 	print_new_line
	pop 	dx;			;restore regs used
	pop 	ax;			;...
  	ret
print_info endp

;------------------------------------------------------------------------
;Procedure:  print_new_line
;
;Function:  prints a new line to std out 
;
;On entry:  none
;
;Returns:  new line to std out
;
;Calls:  none
;------------------------------------------------------------------------
print_new_line proc
  	push 	ax			;backup regs
  	push 	dx			;...
  	mov 	ah, 09h
  	;mov 	dx,offset new_line  	;insert a new line
  	lea	dx, new_line
	int 	21h
  	pop 	dx			;restore regs
  	pop 	ax			;...
  	ret
print_new_line endp

;------------------------------------------------------------------------
;Procedure:  print_ASCIItoHEX
;
;Function:  Print the byte in Al to std out, with a space after the byte
;           
;
;On entry:  al
;
;Returns:  byte with ASCII conversion before int21h to std out
;
;Calls:  none
;------------------------------------------------------------------------
print_ASCIItoHEX proc
  	pusha	
  	
  	xor 	cx, cx			;clean up regs
  	xor 	dx, dx			;...

  	mov 	cl, 04h 		;need to shift high nibble by 4 bits
 
  	mov 	ch, al			;backup our hex value, start work on high nibble
  	and 	ax, 00F0h		;chop off lower nibble
  	shr 	al, cl			;high nibble is now low nibble, cx is free now
  
  	mov 	bx, ax			;prepare for base indexing to ASCII0to9 using bx
 
  	cmp 	bx, 9h 			;need to check for 0 -9, or A - F
  	jg 	high_is_letter		;if this nibble is A - F, skip to is_letter
  
  	mov 	dl, ASCII0to9[bx] 	;check lookup table for our byte
  	jmp 	high_nibble_complete	;finished with high nibble, start work on low nibble

high_is_letter:
  	sub 	bx, 0Ah 		;need to subtract Ah to correctly align byte with 
  	mov 	dl, ASCIIAtoF[bx]	;lookup table

high_nibble_complete: 
  	mov 	ah, 2h			;done with the high nibble, print it
  	int 	21h
  
low_nibble:
  	mov 	al, ch			;grab lower nibble
  	and 	ax, 0Fh			;chop off high nibble
  
  	mov 	bx, ax			;prepare for indexing, using bx
  	cmp 	bx, 9h			;check 0 - 9, or A - F
  	jg 	low_is_letter  		;if this nibble is A - F, skip to is_letter

  	mov 	dl, ASCII0to9[bx]	;converts byte to hex ASCII code
  	jmp 	low_nibble_complete	;if low nibble is number, index into ASCII0to9

low_is_letter:  			;if letter, subtract away 0Ah to correctly index
  	sub 	bx, 0Ah  		;into ASCII0to9
  	mov 	dl, ASCIIAtoF[bx]

low_nibble_complete:  			;finished with low nibble, print
  	mov 	ah, 02
  	int 	21h

  	;mov 	dl, SPACE		;add a space after the byte
  	;int 	21h
    
        popa				;restore regs
        ret
print_ASCIItoHEX endp

;------------------------------------------------------------------------
;Procedure:  print_SPACE
;
;Function:  Prints out a blank space
;           
;
;On entry:  none
;
;Returns:  space to std. out
;
;Calls:  none
;------------------------------------------------------------------------
print_SPACE proc
	push	ax			;backup regs
	push	dx
	mov 	ah, 02
  	mov 	dl, SPACE		;add a space after the byte
  	int 	21h
  	pop	dx
  	pop	ax			;restore regs
  	ret
print_SPACE endp

;------------------------------------------------------------------------
;Procedure:  fill_buffer
;
;Function:  
;           
;
;On entry:  
;
;Returns:  
;
;Calls:  none
;------------------------------------------------------------------------
fill_buffer proc
	push	bx
	push	cx
	
	mov	cx, 256d
	mov	bx,  0
repeat:
	mov	ds:[Data_buff+bx], 2Ah
  	inc	bx
  	loop 	repeat
  	pop	cx
  	pop	bx
  	ret
fill_buffer endp

;real_flat proc near
;	cli				;Do not allow interupts while switching into real flat mode;
;	push	ds			;Save real mode selectors
;	push	es
;	xor	eax, eax		;Patch the GDT psuedo-descriptor
;	mov	ax, ds			;assuming ds points to the segment containing the GDT
;	shl	eax, 4
;	add	[GDT+2], eax
;	lgdt	GDT			;load the GDT
;	mov 	eax, cr0
;	mov 	bx, flat_data		;install only our pmode selector
;	mov 	ds, bx			;install 4GB limits
;	mov	es, bx
;	dec 	ax			;switch back to real mode
;	mov	cr0, eax
;	pop 	es			;restore real mode selectors
;	pop 	ds
;	sti				;enable interupts


;real_flat endp	

CODE_SEG ends
end main