;Phase 5-Project
;Group Members:
;Amal Sarmad 22L-6676
;Wareesha Khawar 22L-6739

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
[org 0x0100]
jmp start


esc_flag: dw 0

test: db 0
number: db 0
oldtimer: dd 0
aniYes: dw 0

m1: db 'Press Enter to Proceed' 
m2: db 'Loading...'
m4: db 'Are you sure you want to leave the game?'
m3: db 'Press Y for Yes and N for No'

inst1: db '1.Press Up key to jump'
inst2: db '2.Collect carrots for score'
inst3: db '3.Blue blocks will break >:)'
inst4: db'Amal Sarmad 22L-6676'
inst5: db'Wareesha Khawar 22L-6739'

dela:      push cx
			mov cx, 0x5555
d1:		loop d1
	
			pop cx
			ret

blue: dw 0
tickcount: dw 0
buffer1: times 132 dw 0
buffer2: times 132 dw 0
vbuffer: times 14 dw 0
totalcells: dw 11352
total_rows: dw 43
total_col: dw 132
divider1: dw 0
divider2: dw 0
oldisr: dd 0

ans: dw 0

rab_att: dw 0x70

rab_x: dw 40
rab_y: dw 69
p1_x: dw 41
p1_y: dw 62
p1_att: dw 0x40
p2_x: dw 29
p2_y: dw 62
p2_att: dw 0x20
p3_x: dw 35
p3_y: dw 62
p3_att: dw 0x30

delayplatform: dw 0xEEE

carrot_x dw 33
carrot_y: dw 62

carr_collect1: dw 0x60
carr_collect2: dw 0x20
score: dw 0
message:db 'Score: ',0
endgame:db 'The game has ended :('

rand: dw 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
random_colour:
push ax
push bx
push cx
push dx

mov word [rand],0
;moves random number from system time in ax
rdtsc

randcheck:
;takes mod with 8
xor dx,dx
mov bx,8
div bx
cmp dx,0 ;black
je randcheck
cmp dx,3; cyan for blue block
je randcheck
cmp dx,7 ;white
je randcheck
cmp dx,4 ;red
je randcheck

cmp dx,1
jne nextc1
mov word [rand],0x10; dark blue
jmp end_rand
nextc1:
cmp dx,2
jne nextc2
mov word [rand],0x20; green
jmp end_rand
nextc2:
cmp dx,5
jne nextc3
mov word [rand],0x50; magenta
jmp end_rand
nextc3:
cmp dx,6
jne nextc4
mov word [rand],0x60; orange
jmp end_rand

nextc4:

end_rand:
pop dx
pop cx
pop bx
pop ax 
ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
random_carrotx:
push ax
push bx
push cx
push dx



randloop:

;moves random number from system time in ax
rdtsc

;takes mod with 6 
xor dx,dx
mov bx,6
div bx

add dx,30;range 30-35 inclusive
mov [rand],dx
cmp dx,33 ;if position of platform is equal to random number,generate again
je randloop
cmp dx,35;if position of platform is equal to random number,generate again
je randloop

pop dx
pop cx
pop bx
pop ax 
ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
random_carroty:
push ax
push bx
push cx
push dx

;moves random number from system time in ax
rdtsc

;takes mod with 12 
xor dx,dx
mov bx,12
div bx

add dx,55;range 55-66 inclusive 55+11=66
mov [rand],dx

pop dx
pop cx
pop bx
pop ax 
ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
random_plat_y:

push ax
push bx
push cx
push dx

;moves random number from system time in ax
rdtsc
 
;takes mod with 9
xor dx,dx
mov bx,9
div bx

add dx,58;range 58-66 inclusive
mov [rand],dx

pop dx
pop cx
pop bx
pop ax 
ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
printnum_BIOS:
 push bp
 mov bp, sp
 push es
 push ax
 push bx
 push cx
 push dx
 push di
 push si
 
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
 
 mov di,0
 
nextdigit1:
 mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
mov [test+di],dl
add di,1
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit1 ; if no divide it again
 
 mov cx,di
 mov si,0
 dec di
 
 reverse:
 mov al,[test+di]
 mov [number+si],al
 inc si
 dec di
 cmp di,-1
 jne reverse
 
 mov si,0x1217
 
 mov ah, 0x13		; service 13 - print string
     
		mov bp, number ;offset of string
		mov al, 1			
		mov bh, 0			; output on page 0
		;mov cx,3  string length
		mov bl, 0x0f ;colour
		mov dx, si; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es	
		; es=ds segment of string
		
		
		INT 0x10

 
 byee:
 pop si
 pop di
 pop dx
 pop cx
 pop bx
 pop ax
 pop es
 pop bp
 ret 2 
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
offset_no:
push bp
mov bp,sp
push ax
push bx

mov ax,[bp+4];row number
mov bx,[total_col]

xor dx,dx

mul bx

add ax,[bp+6];col number
shl ax,1

mov dx,ax

pop bx
pop ax
pop bp
ret 4
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
printnum:
 push bp
 mov bp, sp
 push es
 push ax
 push bx
 push cx
 push dx
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
 push dx ; save ascii value on stack
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit ; if no divide it again
 mov di, 7910 ; point di to top left column
 nextpos: pop dx ; remove a digit from the stack
 mov dh, 0x07 ; use normal attribute
 mov [es:di], dx ; print char on screen
 add di, 2 ; move to next screen location
 loop nextpos ; repeat for all digits on stack
 pop di
 pop dx
 pop cx
 pop bx
 pop ax
 pop es
 pop bp
 ret 2 
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
strlen:
 push bp
 mov bp,sp
 push es
 push cx
 push di
 les di, [bp+4] ; point es:di to string
 mov cx, 0xffff ; load maximum number in cx
 xor al, al ; load a zero in al
 repne scasb ; find zero in the string
 mov ax, 0xffff ; load maximum number in ax
 sub ax, cx ; find change in cx
 dec ax ; exclude null from length
 pop di
 pop cx
 pop es
 pop bp
 ret 4
;----------------------------------------------------------------------------------------------------------------------------------------------------------
printscore:
push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
  
 mov ax, 0xb800
 mov es, ax ; point es to video base
 
 mov ax, 132 ; load al with columns per row
 mul word [bp+8] ; multiply with y position
 add ax, [bp+10] ; add x position
 shl ax, 1 ; turn into byte offset
 
 mov di,ax ; point di to required location
 mov si, [bp+4] ; point si to string
 mov ah, [bp+6] ; load attribute in ah

mov cx,7
  
nextchar1:
mov al,[ds:si]
mov [es:di],ax
add si,1
add di,2
 loop nextchar1 ; repeat for the whole string
 

pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 8 

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

playAnimation:
call ShiftLeft
call ShiftRight
ret
;------------------------------------------------------------------------------------------------------------------------------
delayplat:      push cx
			mov cx, [delayplatform]
de1:		loop de1
			mov cx, [delayplatform]
 de2:		loop de2
			pop cx
			ret
;------------------------------------------------------------------------------------------------------------------------------
delay:      push cx
			mov cx, 0xFFFF
del1:		loop del1
			mov cx, 0xFFFF
 del2:		loop del2
			pop cx
			ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
delay1:      push cx
			mov cx, 0xFFFF
del11:		loop del11
			pop cx
			ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ShiftRight:
push es
push ds
push bp
push ax
push bx
push cx
push dx
push di
push si
push ds

mov ax,0xb800
mov es,ax


mov cx,14
mov si,262
mov di,0

copying:
mov ax,[es:si]
mov [vbuffer+di],ax
add si,264
add di,2
loop copying


mov dx,14
mov ax,es
push ds
mov ds,ax
mov di,262
mov si,260

mov bp,262
mov bx,260

outerR:
mov cx,132

mov si,bx
mov di,bp

innerR:
std
movsw
loop innerR

add bx,264
add bp,264
dec dx
jnz outerR

pop ds

mov cx,14
mov si,0
mov di,0

pasting:
mov ax,[vbuffer+di]
mov [es:si],ax
add si,264
add di,2
loop pasting


	 
pop ds
pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
pop ds
pop es
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------
ShiftLeft:
push ax
push bx
push cx
push dx
push di
push si
push es
push bp
push ds

 mov ax,0xb800
 mov es,ax
 
 mov cx,14
mov si,3960
mov di,0

copying1:
mov ax,[es:si]
mov [vbuffer+di],ax
add si,264
add di,2
loop copying1
 
 

mov dx,14
mov ax,es
push ds
mov ds,ax
mov di,3960
mov si,3962

mov bp,3960
mov bx,3962

outerL:
mov cx,131

mov si,bx
mov di,bp

innerL:
cld
movsw
loop innerL

add bx,264
add bp,264
dec dx
jnz outerL

pop ds
 
 
mov cx,14
mov si,4222
mov di,0

pasting1:
mov ax,[vbuffer+di]
mov [es:si],ax
add si,264
add di,2
loop pasting1

pop ds 
pop bp
pop es
pop si
pop di
pop dx
pop cx
pop bx
pop ax
ret

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
drawTrees:
    push bp
    mov bp,sp
    push ax
	mov ax,[bp+4]
	
	push 0x40
	push 0x2
	push 0x2
	push ax
	call drawRectangle
	
	mov ax,[bp+4]
	sub ax,270
	
	push 0x20
	push 0x8
	push 0x1
	push ax
	call drawRectangle
	
	mov ax,[bp+4]
	sub ax,532
	
	push 0x20
	push 0x6
	push 0x1
	push ax
	call drawRectangle
	
	mov ax,[bp+4]
	sub ax,794
	
	push 0x20
	push 0x4
	push 0x1
	push ax
	call drawRectangle
		
	pop ax	
	mov sp,bp
	pop bp
	ret 2
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

drawBoat:
		push bp
		mov bp,sp
		push ax
;starting offset for boat parameter
		mov ax,[bp+4]

		push 0x60    ;color
		push 0x10    ;width
		push 0x2     ;length
		push ax

		call drawRectangle

		mov ax,[bp+4]
		sub ax,274

		push 0x60
		push 0x19
		push 0x1
		push ax

		call drawRectangle

		mov ax,[bp+4]
		sub ax,1036

		push 0x70
		push 0x1
		push 0x3
		push ax

		call drawRectangle

		mov ax,[bp+4]
		sub ax,1034

		push 0x40
		push 0x4
		push 0x2
		push ax

		call drawRectangle

		pop ax
		mov sp,bp
		pop bp
		ret 2


;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
drawRectangle:
		push bp
		mov bp,sp
		sub sp,2
		push di
		push si
		push ax
		push bx
		push cx
		push dx
		push es
;point to set video
		mov ax,0xb800
		mov es,ax

		mov di,[bp+4] ;starting position
		mov ax,[bp+6] ;length
		mov [bp-2],ax
		mov bx,[bp+8] ;width


		mov dx,[bp-2];length

		mov ah,[bp+10];colour
		mov al,0x20

		outer:
		 mov cx,bx
		 mov si,di

		 inner:
		  mov [es:si],ax
		  add si,2
		  dec cx
		  jnz inner

		 add di,[total_col]
		 add di,[total_col]

		 dec dx
		jnz outer

		pop es
		pop dx
		pop cx
		pop bx
		pop ax
		pop si
		pop di
		add sp,2
		mov sp,bp
		pop bp 
		ret 8

;-------------------------------------------------------------------------------------------------------------------------------------------------------
drawBuilding:
		push bp
		mov bp,sp
		sub sp,4
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

		push word [bp+10]
		push word [bp+8]
		push word [bp+6]
		push word [bp+4]

		call drawRectangle

		mov ax,[bp+8];width
		mov bx,3
		mov dx,0
		div bx
		sub ax,1

		mov [bp-2],ax ;column space for  windows

		mov ax,[bp+6];length
		mov bx,3
		mov dx,0
		div bx


		mov [bp-4],ax ;row space for  windows

		mov di,[bp+4]

		mov cx,[bp-4]
		mov dx,[bp-2]
		mov bx,[bp+14]

		add di,[total_col]
		add di,[total_col]

		outer1:;increment row
		 mov ax,[bp+12]
		 mov si,di
		 inner1:;increment column
			mov dx,[bp-2]
			
			k:
			 add si,2
			 dec dx
			jnz k

			push 0x70
			push 1
			push 1
			push si
			call drawRectangle
			dec ax
			jnz inner1
			
		 mov cx,[bp-4]
		 w:
		  add di,[total_col]
		  add di,[total_col]
		  dec cx
		 jnz w 

		 dec bx
		jnz outer1


		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		add sp,4
		mov sp,bp
		pop bp
		ret 12

;-------------------------------------------------------------------------------------------------------------------------------------------------------
background:

		push bp
		mov bp,sp
		push dx
		push cx
		push bx
		push ax
		push es
		push di
		push si

		mov cx,[divider1]
		shr cx,1  

		mov ax,0xb800
		mov es,ax
		mov di,0
		mov ax,0x1020

		;colours background to dark blue
		colorbk:
		mov [es:di],ax
		add di,2
		dec cx
		jnz colorbk

		;drawing buildings

		push 0x3
		push 0x3
		push 0x60
		push 10
		push 14
		push 0
		call drawBuilding


		push 0x3
		push 0x3
		push 0x50
		push 0x10
		push 0x9
		push 1340
		call drawBuilding



		push 0x3
		push 0x3
		push 0x40
		push 0x9
		push 0x10
		push 844
		call drawBuilding

		push 0x3
		push 0x3
		push 0x20
		push 0x9
		push 0x10
		push 1390
		call drawBuilding

		push 0x3
		push 0x3
		push 0x60
		push 0x9
		push 0x10
		push 616
		call drawBuilding

		push 0x2
		push 0x3
		push 0x40
		push 0x9
		push 0x10
		push 742
		call drawBuilding



		push 0x4
		push 0x3
		push 0x50
		push 0x10
		push 0x9
		push 1552
		call drawBuilding


		push 0x3
		push 0x3
		push 0x20
		push 0x9
		push 0x10
		push 1252
		call drawBuilding


		push 0x2
		push 0x3
		push 0x60
		push 0x9
		push 0x10
		push 706
		call drawBuilding

		push 0x2
		push 0x3
		push 0x60
		push 0x9
		push 0x10
		mov ax,616
		push ax
		call drawBuilding

		push 0x2
		push 0x3
		push 0x50
		push 0x9
		push 0x10
		mov ax,1426
		push ax
		call drawBuilding
		
		push 0x2
		push 0x3
		push 0x40
		push 0x9
		push 0x10
		mov ax,1180
		push ax
		call drawBuilding
	
		
		;drawing trees
		push 3272
		call drawTrees
		
		push 3342
		call drawTrees
		
		push 3422
		call drawTrees
		
		push 3184
		call drawTrees
		
		pop si
		pop di
		pop es
		pop ax
		pop bx
		pop cx
		pop dx
		pop bp

		ret

;-------------------------------------------------------------------------------------------------------------------------------------------------------
foreground:
;stores old value of registers
		push bp
		push ax
		push bx
		push cx
		push dx
		push es
		push di
		push si

		;points at videobase
		mov ax,0xb800
		mov es,ax
		;total no of cells in divider
		mov di,[divider1]
		add di,[total_col]     ;2 bytes
		add di,[total_col]

		;colour for foreground
		mov ax,0x3020

		;position of 2nd divider starts after end of first divider
		mov cx,[divider1]
		shr cx,1
		sub cx,[total_col]

		;colours foreground
		colorfk:
		mov [es:di],ax
		add di,2
		dec cx
		jnz colorfk

		;position of 2nd divider2
		mov cx,[divider2]
		add cx,[total_col]
		add cx,[total_col]

		;for ten waves per line, total no of columns are divided by 10 and space is decided accordingly
		mov ax,[total_col]
		mov bx,10
		mov dx,0
		div bx
		mov si,ax
		mov bx,ax
		add bx,1
		;position of waves start from end of second divider
		mov di,[divider1]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		;texture for waves
		mov ax,0x3f5e 
		mov bp,[total_col]

		;printswaves
		printWaves:
		 cmp si,0
		 jnz skip4
		 ;prints three waves in a row
		 mov [es:di],ax
		 add di,2
		 mov [es:di],ax
		 add di,2
		 mov [es:di],ax
		 ;restores value of di
		 sub di,2
		 sub di,2
		 mov si,bx;si=13

		 skip4:

		 sub si,1
		 add di,2
		 dec bp

		jnz skip5
		;skips five rows for realistic effect of waves
		mov bp,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]
		add di,[total_col]

		skip5:
		cmp di,cx
		jb printWaves


		;prints boat
		push 5460
		call drawBoat
;restores old values of registers
		pop si
		pop di
		pop es
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
clrscr:		
        push es
	    push ax
        push di

		mov ax, 0xb800
		mov es, ax					; point es to video base
		mov di, 0					; point di to top left column
		mov ah, 0x07
		mov al, 0x20

        nextloc:	
            mov word [es:di], AX	; clear next char on screen
			add di, 2					; move to next screen location
			cmp di,	[totalcells]          		; 132x43x2
			jb nextloc					; if no clear next position

		pop di
		pop ax
		pop es
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
dividers:
   
    push es
    push ax
    push bx
    push cx
    push dx 
    push di

;pointing to videobase
   mov ax,0xb800
   mov  es,ax
   
   ;132*rows/3+col   1/3 divider
	mov ax,0x2B ;43
	mov bx,0x3
	xor dx,dx ;dx=0
	
	div bx
	mov bx,ax
	
	mov  ax, [total_col]
	mul bx
	shl ax,1
	
	mov [divider1],ax
	mov di,ax

    mov ah, 0x07
    mov al, '-'
	
	mov cx,[total_col]
	
loop1:
    mov [es:di], ax
    add di,2
    dec cx
    jnz loop1
	
	
	  ;132*rows*2/3+col  2/3 divider
	mov ax,0x2B
	shl ax,1
	mov bx,0x3
	xor dx,dx
	
	div bx
	mov bx,ax
	
	mov  ax, [total_col]
	mul bx
	shl ax,1
	
	mov [divider2],ax
	mov di,ax

    mov ah, 0x07
    mov al, '-'
	;one row has 132 columns and to go from 0th column to 131st column
	mov cx,[total_col]
	
loop2:
mov [es:di], ax
    add di,2
    dec cx
    jnz loop2

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
   
    ret 
	
;-------------------------------------------------------------------------------------------------------------------------------------------------------	
printMainScreen:
 
  call dividers
  call background
  call foreground
  call dividers
  ret
	;------------------------------------------------------------------------------------------------------------------------------------------------------------
platform_animation:	
push ax
push cx

mov word [p2_y],62
mov ax,[rab_x]
add ax,1;checks if rabbit is on platform no 2

rightplt:
cmp word ax,[p2_x]
jne skip8
add word [rab_y],1
skip8:
add word [p2_y],1
call print_gameScreen
call delayplat
call delayplat
call check_carrot
cmp word [p2_y],82
jne rightplt

mov cx,40
leftplt:
	
cmp word ax,[p2_x]
jne skip7
sub word [rab_y],1
skip7:
sub word [p2_y],1
call print_gameScreen
call delayplat
call delayplat
call check_carrot
loop leftplt

rightpl:
cmp word ax,[p2_x]
jne skip6
add word [rab_y],1
skip6:
add word [p2_y],1
call print_gameScreen
call delayplat
call delayplat
call check_carrot
cmp word [p2_y],62
jne rightpl

pop cx
pop ax
ret	
;//////////////////////////////////
platform_animation1:	
push ax
push cx

mov word [p1_y],62
; mov ax,[rab_x]
; add ax,1;checks if rabbit is on platform no 2

rightplt1:
; cmp word ax,[p2_x]
; jne skip8
; add word [rab_y],1
;skip8:
add word [p1_y],1
call print_gameScreen
call delayplat
;call delayplat
;call check_carrot
cmp word [p1_y],82
jne rightplt1

mov cx,40
leftplt1:
	
; cmp word ax,[p2_x]
; jne skip7
; sub word [rab_y],1
;skip7:
sub word [p1_y],1
call print_gameScreen
call delayplat
;call delayplat
;call check_carrot
loop leftplt1

rightpl1:
; cmp word ax,[p2_x]
; jne skip6
; add word [rab_y],1
; skip6:
add word [p1_y],1
call print_gameScreen
call delayplat
;call delayplat
;call check_carrot
cmp word [p1_y],62
jne rightpl1

pop cx
pop ax
ret	
;------------------------------------------------------------------------------------------------------------------------------------------------------------

checkifRabbitonPlat:
push ax
push es
push di
push dx

mov cx,0 ;false

;calculates position of offset below rabbit
mov ax,[rab_x]
add ax,1

push word [rab_y]
push ax
call offset_no

;checks if space below rabbit is black 
mov di,dx

mov ax,0xb800
mov es,ax

add di,8;go to tail
cmp word [es:di],0x0020;black background
jne true
sub di,2;check if platform is there for each comparision
cmp word [es:di],0x0020
jne true
sub di,2
cmp word [es:di],0x0020
jne true
sub di,2
cmp word [es:di],0x0020
jne true
sub di,2
cmp word [es:di],0x0020
jne true
jmp skip

true:
mov cx,1;true

skip:

pop dx
pop di
pop es
pop ax
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
check_bluebrick:
push ax
push es
push di
push dx

;calculate offset of position below rabbit

mov word [blue],0 ;false
mov word [tickcount],0

mov ax,[rab_x]
add ax,1

push word [rab_y]
push ax
call offset_no


;checks if colour of space below rabbit is blue
mov di,dx

mov ax,0xb800
mov es,ax

add di,8;go to tail
cmp word [es:di],0x3020;blue platform check
je true1
sub di,2;check if platform is there for each comparision
cmp word [es:di],0x3020
je true1
sub di,2
cmp word [es:di],0x3020
je true1
sub di,2
cmp word [es:di],0x3020
je true1
sub di,2
cmp word [es:di],0x3020
je true1
jmp skip11

;if the platform is blue, move true in variable later to be used in timer
true1:
mov word [blue],1;true

skip11:
pop dx
pop di
pop es
pop ax
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
jump_animation:
push ax
push cx
push es

mov ax,0xb800
mov es,ax

mov cx,6
mov word [rab_att],0x10;dark blue

jumploop:
call delay
sub word [rab_x],1;row is decrementing to make rabbit go up
call check_carrot
call print_gameScreen
loop jumploop

mov word [rab_att],0x70;white

pop es
pop cx
pop ax
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
displayEndgame:
push ax
push bx
push cx
push dx
push bp
push es
call clrscr
mov ax, 0x000D ; set 320x200 graphics mode
int 0x10 ; bios video services

mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f
		mov cx, 21
		mov dx, 0x0f0a; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, endgame;offset of string
		
		INT 0x10
		
		
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f
		mov cx, 6
		mov dx, 0x1210; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, message;offset of string
		
		INT 0x10
		
		push word [score]
		call printnum_BIOS
		
		
pop es
pop bp
pop dx
pop cx
pop bx
pop ax
ret
;-----------------------------------------------------------------------------------------------------------------
print_platform3:
pusha
mov ax,[p3_y]
push ax
mov ax,[p3_x]
push ax
call offset_no
;platform 1

mov ax,word [p3_att]
push ax
push 16
push 1
push dx
call drawRectangle
popa 
ret
;-----------------------------------------------------------------------------------------------------------------
print_platform2:
push ax
push dx
;platform 2
mov ax,[p2_y]
push ax
mov ax,[p2_x]
push ax
call offset_no

push word [p2_att]
push 16
push 1
push dx
call drawRectangle
pop dx 
pop ax
ret

;-----------------------------------------------------------------------------------------------------------------
print_platform1:
push ax
push dx

mov ax,[p1_y]
push ax
mov ax,[p1_x]
push ax
call offset_no
;platform 1

mov ax, word [p1_att]
push ax
push 16
push 1
push dx
call drawRectangle
pop dx
pop ax
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
drawRabbit:
push ax
push dx

mov ax,[rab_y]
push ax
mov ax,[rab_x]
push ax
call offset_no

push word [rab_att]
push 4
push 1
push dx
call drawRectangle

sub dx,264

push word [rab_att]
push 1
push 1
push dx
call drawRectangle

sub dx,264

push word [rab_att]
push 1
push 1
push dx
call drawRectangle

add dx,264
add dx,264
add dx,8

push 0x50
push 1
push 1
push dx
call drawRectangle

sub dx,264
sub dx,10

push word [rab_att]
push 1
push 1
push dx
call drawRectangle

pop dx
pop ax
ret

;-------------------------------------------------------------------------------------------------------------------------------------------------------
drawCarrot:
push ax
push dx

mov ax,[carrot_y]
push ax
mov ax,[carrot_x]
push ax
call offset_no

push word [carr_collect1]
push 4
push 1
push dx
call drawRectangle

push word [carr_collect2]
push 1
push 1
push dx
call drawRectangle

pop dx
pop ax
ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------
print_gameScreen: 
push bp
mov bp,sp
push ax
push bx
push cx
push dx
push di
push es
	 

mov ax,0xb800
mov es,ax

;background3
mov di,7656
mov ax,0x0020

bck3:
mov [es:di],ax
add di,2
cmp di,11352
jnz bck3
	 

call print_platform1
call print_platform2
call print_platform3
	 
	 


;carrot
call drawCarrot
;rabbit
call drawRabbit

;score
    push 120
    push 29
	 mov ax, 0x07
	 push ax ; push attribute
	 mov ax, message
	 push ax ; push offset of string
	 call printscore ; print the string
	 
	 push word [score]
	 call printnum
	 
;truth value to play animation or else skip if leave game message is showing
cmp word [aniYes],0
jne bye
	
bye:
pop es
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
ret

;-------------------------------------------------------------------------------------------------------------------------------------------------------
check_carrot:
push ax
push bx
push cx
push dx
push es
push di

mov ax, [rab_x]
mov bx,[rab_y]

;checks if same row else skip
cmp ax,[carrot_x]
jne skips

;checks if carrot is present 7+y position from rabbit's reference y point for body
mov cx,7
checkingcarr:
cmp bx,[carrot_y]
je true2
add bx,1
loop checkingcarr

;checks if carrot is present y-6 position from rabbit's reference y point for head
mov bx,[rab_y]
mov cx,6
checkingcarr1:
cmp bx,[carrot_y]
je true2
sub bx,1
loop checkingcarr1

jmp skips

;if rabbit collects carrot, move black into colour attribute, increase score and decrease delay
true2: 
;checks if carrot is already collected or no
cmp word  [carr_collect1],0x00
je skips
add word [score],10;increase score by 10
sub word [delayplatform],0xFF;increase speed of platform
mov word [carr_collect1],0x00;black from orange
mov word [carr_collect2],0x00;black from green

;generates carrot at random position if collected
call random_carroty
mov dx,[rand]
mov [carrot_y],dx
call random_carrotx
mov dx,[rand]
mov [carrot_x],dx


skips:
pop di
pop es
pop dx
pop cx
pop bx
pop ax
ret
;---------------------------------------------------------------------------------------------------------------------------------
leavegame:

push ax
push bx
push cx
push dx
push es
push di

call clrscr
mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0d
		mov cx, 40
		mov dx, 0x0a2a
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m4; bp = offset of string
		
		INT 0x10
		
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0d
		mov cx, 28
		mov dx, 0x0b30
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m3; bp = offset of string
		
		INT 0x10
		
		;stops animation so message doesnt move
		mov word [aniYes],1
		
		mov word [esc_flag],1
		
pop di
pop es
pop dx
pop cx
pop bx
pop ax
ret
		
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------------
rabbit_jumped:
 push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds
	
	
;restores colour of blue brick
mov word [p3_att],0x30

call jump_animation
call delay
call delay

;checks if rabbit has landed on platform and returns true or false value in cx
call checkifRabbitonPlat
cmp cx,1;returns bool value in cx
jne end_game

;checks if platform is blue
call check_bluebrick

;scroll down screen
mov cx,6
down:
add word [rab_x],1
add word [p1_x],1
add word [p2_x],1
add word [p3_x],1
add word [carrot_x],1

call print_gameScreen

call delay
call delay
loop down

;if platform goes below screen, generate it again on top at random column position
cmp word [p1_x],42
jnge skipplt1

mov word [p1_x],29

;random column generation for platform 1
call random_plat_y
mov dx,[rand]
mov word [p1_y],dx

;random colour generation for platform 1
xor dx,dx
call random_colour
mov dx,[rand]
mov [p1_att],dx
xor dx,dx

jmp skiplt3

skipplt1:
cmp word [p2_x],42
jnge skipplt2
mov word [p2_x],29
mov word [p2_y],62

jmp skiplt3

skipplt2:
mov word [p3_x],29
;random column generation for platform 3
call random_plat_y
mov dx,[rand]
mov word [p3_y],dx

;after carrot is collected, restore colour attribute regardless after jump
skiplt3:
mov word [carr_collect1],0x60;orange
mov word [carr_collect2],0x20;green top

;if carrot goes below screen, generate again at random x position
cmp word [carrot_x],42
jnge end_rabbit
call random_carrotx
mov dx,[rand]
mov word [carrot_x],dx

end_rabbit:
 pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
	
;-------------------------------------------------------------------------------------------------------------------------------------------------------
;keyboard interrupt service routine
kbisr:
push ax
push es
 
    mov ax, 0xb800
    mov es, ax
	
    in al, 0x60 
	
	;up key scancode
    cmp al, 0x48
    jne esckey

;routine when up key is pressed
mov al,0x20
out 0x20,al

call rabbit_jumped
jmp exit

;if esc key is pressed
esckey:
cmp al,1;scancode of esc key
jne ykey
call leavegame
jmp exit

ykey:
cmp al,0x15;scancode of y key
jne nkey
cmp word [esc_flag],1
jne nomatch
pop es
pop ax
mov al, 0x20;end of interupt signal as game will end
out 0x20, al
call end_game


nkey: 
cmp al,0x31;scancode of n key
jne nomatch
cmp word [esc_flag],1
jne nomatch
call printMainScreen
mov word [aniYes],0
mov word [esc_flag],0
jmp exit
;if keys dont match, jump back to old interrupt service routine
nomatch:	
    pop es
	pop ax
    jmp far [cs:oldisr] ; call the original ISR
exit:		
    mov al, 0x20;end of interupt signal
    out 0x20, al 
    pop es
	pop ax
    iret 
;-------------------------------------------------------------------------------------------------------------------------------	
;leaves game	
end_game: 
	call displayEndgame
	jmp end_program
	
;----------------------------------------------------------------------------------------------------------------
; timer interrupt service routine
timer:
pusha
cmp word [blue],1
jne exit2
add word [tickcount],1
cmp word [tickcount],50; 3 secs as 1 sec is equal to 18 tickcounts
jne skiptime
mov al, 0x20
out 0x20, al ; end of interrupt
popa
call displayEndgame
jmp end_program
 
 skiptime:
 mov ax,[tickcount]
 mov bx,2
 xor dx,dx
 div bx
 cmp dx,0
 jne blu
 mov word [p3_att],0x10
 jmp exit2
 blu:
 mov word [p3_att],0x30
 exit2:
 cmp word [aniYes],1
 je exit_timer

mov cx,2
anii:
 call ShiftLeft
 call ShiftRight
 loop anii

 exit_timer:
 mov al, 0x20
 out 0x20, al ; end of interrupt
 popa
 iret ; return from interrupt

;------------------------------------------------------------------------------------------------------------------------------------------------------------
startingscreen:
pusha
push bp
mov ax, 0x000D ; set 320x200 graphics mode
int 0x10 ;video services

;colours background pink
mov ah,0bh
mov bh,00h
mov bl,05h ;colour
int 0x10

;loops to print jump in dull white

;**************************************************************************************************
mov ax, 0x0C07 ; put pixel in white color
xor bx, bx ; page number 0
mov cx, 120 ; x position 120
mov dx, 50 ; y position 50

g1:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne g1
 
 g2:
  int 0x10 ; bios video services
 sub cx,1
 cmp cx,100
 jne g2
 
 g3:
 int 0x10
 sub dx,1
 cmp dx,90
 jne g3
 
 mov cx,130
 mov dx,50
 
 g4:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne g4
 
  
 g5:
  int 0x10 ; bios video services
 add cx,1
 cmp cx,150
 jne g5
 
 g6:
 int 0x10
 sub dx,1
 cmp dx,50
 jne g6
 
  mov cx,160
 mov dx,50

g7:
int 0x10
add dx,1
cmp dx,100
jne g7

  mov cx,160
 mov dx,50

g8:
int 0x10
add dx,1
add cx,1
cmp dx,70
jne g8

g9:
int 0x10
sub dx,1
add cx,1
cmp dx,50
jne g9

g10:
int 0x10
add dx,1
cmp dx,100
jne g10

 mov cx,210
 mov dx,50
 
 g11:
 int 0x10
 add dx,1
cmp dx,100
jne g11

 mov cx,210
 mov dx,50

g12:
int 0x10
add cx,1
cmp cx,230
jne g12

g13:
int 0x10
add dx,1
cmp dx,70
jne g13

g14:
int 0x10
sub cx,1 
cmp cx,210
jne g14

;loops to print jump in bright white

;**************************************************************************************************
mov ax, 0x0C0f; put pixel in white color
xor bx, bx ; page number 0
mov cx, 120 ; x position 200
mov dx, 50 ; y position 200


h1:
call dela
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne h1
 
 h2:
 call dela
  int 0x10 ; bios video services
 sub cx,1
 cmp cx,100
 jne h2
 
 h3:
 call dela
 int 0x10
 sub dx,1
 cmp dx,90
 jne h3
 
 mov cx,130
 mov dx,50
 
 h4:
 call dela
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne h4
 
  
 h5:
 call dela
  int 0x10 ; bios video services
 add cx,1
 cmp cx,150
 jne h5
 
 h6:
 call dela
 int 0x10
 sub dx,1
 cmp dx,50
 jne h6
 
  mov cx,160
 mov dx,50

h7:
call dela
int 0x10
add dx,1
cmp dx,100
jne h7

  mov cx,160
 mov dx,50

h8:
call dela
int 0x10
add dx,1
add cx,1
cmp dx,70
jne h8

h9:
call dela
int 0x10
sub dx,1
add cx,1
cmp dx,50
jne h9

h10:
call dela
int 0x10
add dx,1
cmp dx,100
jne h10

 mov cx,210
 mov dx,50
 
 h11:
 call dela
 int 0x10
 add dx,1
cmp dx,100
jne h11

 mov cx,210
 mov dx,50

h12:
call dela
int 0x10
add cx,1
cmp cx,230
jne h12

h13:
call dela
int 0x10
add dx,1
cmp dx,70
jne h13

h14:
call dela
int 0x10
sub cx,1 
cmp cx,210
jne h14

;prints 'press enter to proceed'

;**************************************************************************************************
mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f;colour
		mov cx, 22 
		mov dx, 0x0f0a		; row 15 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m1	; bp = offset of string
		
		INT 0x10

;gets character	
mov ah,0
int 0x16

;recolours pixels of jump to pink to match background
;**************************************************************************************************
mov ax, 0x0C05; put pixel in white color
xor bx, bx ; page number 0
mov cx, 120 ; x position 200
mov dx, 50 ; y position 200


hh1:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne hh1
 
 hh2:
  int 0x10 ; bios video services
 sub cx,1
 cmp cx,100
 jne hh2
 
 hh3:
 int 0x10
 sub dx,1
 cmp dx,90
 jne hh3
 
 mov cx,130
 mov dx,50
 
 hh4:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne hh4
 
  
 hh5:
  int 0x10 ; bios video services
 add cx,1
 cmp cx,150
 jne hh5
 
 hh6:
 int 0x10
 sub dx,1
 cmp dx,50
 jne hh6
 
  mov cx,160
 mov dx,50

hh7:
int 0x10
add dx,1
cmp dx,100
jne hh7

  mov cx,160
 mov dx,50

hh8:
int 0x10
add dx,1
add cx,1
cmp dx,70
jne hh8

hh9:
int 0x10
sub dx,1
add cx,1
cmp dx,50
jne hh9

hh10:
int 0x10
add dx,1
cmp dx,100
jne hh10

 mov cx,210
 mov dx,50
 
 hh11:
 int 0x10
 add dx,1
cmp dx,100
jne hh11

 mov cx,210
 mov dx,50

hh12:
int 0x10
add cx,1
cmp cx,230
jne hh12

hh13:
int 0x10
add dx,1
cmp dx,70
jne hh13

hh14:
int 0x10
sub cx,1 
cmp cx,210
jne hh14

;recolours string to pink to match background
;**************************************************************************************************

mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x05
		mov cx, 22 
		mov dx, 0x0f0a		; row 15 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m1	; bp = offset of string
		
		INT 0x10
		
		
		
	mov ah,0
	int 0x16
	
;**************************************************************************************88
	mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f
		mov cx, 22
		mov dx, 0x0608		; row 6 column 8
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst1	; bp = offset of string
		
		INT 0x10
		
	
	mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f
		mov cx, 27
		mov dx, 0x0808	; row 8 column 8
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst2	; bp = offset of string
		
		INT 0x10
;**************************************************************************************
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0f
		mov cx, 28
		mov dx, 0x0a08	; row 10 column 8
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst3	; bp = offset of string
		
		INT 0x10
;******************************************************************************************
			mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 20 
		mov dx, 0x100a		; row 16 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst4	; bp = offset of string
		
		INT 0x10
;******************************************************************************************		
			mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 24
		mov dx,0x120a	; row 18 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst5	; bp = offset of string
		
		INT 0x10
;******************************************************************************************		
	mov ah,0
	int 0x16
	
	mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 22
		mov dx, 0x0608		; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst1	; bp = offset of string
		
		INT 0x10
;******************************************************************************************			
	mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 27
		mov dx, 0x0808	; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst2	; bp = offset of string
		
		INT 0x10
;******************************************************************************************		
		mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 28
		mov dx, 0x0a08	; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, inst3	; bp = offset of string
		
		INT 0x10
;******************************************************************************************
	
;colours background to pink again for instructions screen	
mov ah,0bh
mov bh,00h
mov bl,05h
int 0x10

;recolours pixels of jump to green to match background
;**************************************************************************************************
mov ax, 0x0C0a ; put pixel in green color
xor bx, bx ; page number 0
mov cx, 120 ; x position 200
mov dx, 50 ; y position 200

i1:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne i1
 
 i2:
  int 0x10 ; bios video services
 sub cx,1
 cmp cx,100
 jne i2
 
 i3:
 int 0x10
 sub dx,1
 cmp dx,90
 jne i3
 
 mov cx,130
 mov dx,50
 
 i4:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,100
 jne i4
 
  
 i5:
  int 0x10 ; bios video services
 add cx,1
 cmp cx,150
 jne i5
 
 i6:
 int 0x10
 sub dx,1
 cmp dx,50
 jne i6
 
  mov cx,160
 mov dx,50

i7:
int 0x10
add dx,1
cmp dx,100
jne i7

  mov cx,160
 mov dx,50

i8:
int 0x10
add dx,1
add cx,1
cmp dx,70
jne i8

i9:
int 0x10
sub dx,1
add cx,1
cmp dx,50
jne i9

i10:
int 0x10
add dx,1
cmp dx,100
jne i10

 mov cx,210
 mov dx,50
 
 i11:
 int 0x10
 add dx,1
cmp dx,100
jne i11

 mov cx,210
 mov dx,50

i12:
int 0x10
add cx,1
cmp cx,230
jne i12

i13:
int 0x10
add dx,1
cmp dx,70
jne i13

i14:
int 0x10
sub cx,1 
cmp cx,210
jne i14	

;recolours string to green to match background 
;**************************************************************************************************
mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0a
		mov cx, 22 
		mov dx, 0x0f0a		; row 15 column 10
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m1; bp = offset of string
		
		INT 0x10

;colours background green
;**************************************************************************************************
mov ah,0bh
mov bh,00h
mov bl,0ah
int 0x10

;prints loading
;**************************************************************************************************
mov ah, 0x13		; service 13 - print string
		
		mov al, 1			
		mov bh, 0			; output on page 0
		
		mov bl, 0x0d
		mov cx, 10
		mov dx, 0x0a0f	; row 10 column 3
		
		;es:bp = ds:message
		push ds
		pop es				; es=ds segment of string
		mov bp, m2		; bp = offset of string
		
		INT 0x10

;colours outline of rectangle for loading screen
;**************************************************************************************************
mov ax, 0x0C0f ; put pixel in white color
xor bx, bx ; page number 0
mov cx, 100 ; x position 200
mov dx, 100 ; y position 200

rec1:
 int 0x10 ; bios video services
 add dx,1
 cmp dx,110
 jne rec1
 
 rec2:
 int 0x10 ; bios video services
 add cx,1
 cmp cx,220
 jne rec2
 
  rec3:
 int 0x10 ; bios video services
 sub dx,1
 cmp dx,100
 jne rec3
 
 rec4:
  int 0x10 ; bios video services
 sub cx,1
 cmp cx,100
 jne rec4
 
mov cx, 100 ; x position 200
mov dx, 100 ; y position 200

;fills rectangle for loading screen
ot1:
add cx,1
mov dx,100
 fill1:
 int 0x10
 add dx,1
 cmp dx,110
 jne fill1
 cmp cx,200
 jne hee
 ;fakes lag for loading screen for certain column position
 call delay
 call delay
 call delay
 call delay
 call delay
 hee:
 call delay
 cmp cx,220
 jne ot1

pop bp
popa
ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------
start:	
; following code just changes your screen  resolution to 43x132 Mode
call startingscreen
    mov ah,0x00
    mov al, 0x54
    int 0x10
	
	call printMainScreen
	
	mov ah,0x01
	int 0x21		

	xor ax, ax
    mov es, ax
	
	cli ;disable interrupts
    mov ax, [es:9*4]
    mov [oldisr], ax 
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax 
	
	mov ax, [es:8*4]
    mov [oldtimer], ax 
    mov ax, [es:8*4+2]
    mov [oldtimer+2], ax 
    sti;enable interrupts

    cli ;disable interrupts
    mov word [es:9*4], kbisr 
    mov [es:9*4+2], cs 
	
	mov word [es:8*4], timer; store offset at n*4
    mov [es:8*4+2], cs ; store segment at n*4+2
    sti ;enable interrupts

    call clrscr
	game1:
	call printMainScreen
    call print_gameScreen
	
    game:
    call platform_animation
	call platform_animation1
    jmp game
	
end_program:

mov ah,0
int 0x16

;converting back to text mode
    mov ah,0x00
    mov al, 0x54
    int 0x10
	
	;unhooking interrupts
	mov ax,0
	mov es,ax
	
	cli
	  mov ax,[oldisr]
	  mov [es:9*4],ax
	  
	  mov ax,[oldisr+2]
	  mov [es:9*4+2],ax
		
	   mov ax,[oldtimer]
	  mov [es:8*4],ax
	  
	  mov ax,[oldtimer+2]
	  mov [es:8*4+2],ax
	sti
		
	
	
 mov ax, 0x4c00 ; terminate 
 int 0x21