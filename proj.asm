

; 		2 EXTRA FEATURES

;		BONUS FOOD After every 4 Normal Foods that results in increasing 5 Score Points
;		SPEED INCREASE After every Bonus Food eaten


[org 0x0100]
jmp start

			; string messages for display
msg1: db 'SCORE: '
msg2: db '<< WELCOME TO SNAKE GAME >>'
gameOverMsg: db '*** GAME OVER ***'
restartOrExitMsg: db 'Press ENTER to Restart or Press SPACEBAR to Exit the Game '
endingMsg: db 		'Thanks For Playing. Have a Nice Day ! '

;--------------------------------------------------------------------------------------------------

clrscr:
	push bp
	mov bp,sp
	push ax
	push cx
	push es
	push di
	

	mov ax,0xb800
	mov es,ax
	mov di,0
	
	mov cx,2000
	mov ax,0x0720
	rep stosw

	pop di
	pop es
	pop cx
	pop ax
	pop bp

	ret

;---------------------------------------------------------------------------------------------------------

printstr:
	
	push bp
	mov bp,sp
	push ax
	push cx
	push es
	push ds
	push si
	push di
	
	mov ax,0xb800
	mov es,ax
	mov al,80
	mul byte [bp+6] 	; rows
	add ax,[bp+4] 		; cols
	mov di,ax
	shl di,1

	mov si,[bp+8] 		; string address
	mov cx,[bp+10] 		; size

l1:
	lodsb 
	mov ah,7
	stosw
	loop l1

	pop di
	pop si
	pop ds
	pop es
	pop cx
	pop ax
	pop bp

	ret 8

;--------------------------------------------------------------------------------------------------------

printScore:
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	
	mov ax,0xb800
	mov es,ax

	mov ax,[score]
	
	mov bx,10
	mov cx,0

nextdigit:
	mov dx,0
	div bx
	add dl,0x30
	push dx
	inc cx
	cmp ax,0
	jnz nextdigit

	mov di,776

nextpos:
	pop dx
	mov dh,0x07
	mov [es:di],dx
	add di,2
	loop nextpos


	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret	

;-------------------------------------------------------------------------------------------------------

printsq:
	push ax
	push dx
	push cx
	push di
	push es
	mov ax,0xb800
	mov es,ax
	mov dl,0xC4
	mov dh,0x07
	
	mov di,990
	mov cx,30
topRow:
	mov [es:di],dx
	add di,2
	loop topRow

	mov dl,0xB3
	mov cx,14
	mov di,1148
leftClm:
	mov [es:di],dx
	add di,160
	loop leftClm
	
	mov dl,0xC4
	mov cx,30
	mov di,3390
	
bottomRow:
	mov [es:di],dx
	add di,2
	loop bottomRow
	
	mov dl,0xB3
	mov cx,14
	mov di,1210

rightClm:
	mov [es:di],dx
	add di,160
	loop rightClm
	
	pop es
	pop di
	pop cx
	pop dx
	pop ax

	ret
	
;-----------------------------------------------------------------------------------------------------

SnakeDraw:
	
	push ax
	push es
	push cx
	push si
	push dx
	push di
	push bx


	mov ax,0xb800
	mov es,ax
	
	mov cx,[snakelength]
	xor si,si

OffsetCalHead:
	mov bl,[snakeOffsets+si]		; row
	mov bh,[snakeOffsets+si+1]		; clm
	
	mov al,80
	mul bl
	add al,bh
	shl ax,1
	mov di,ax
	

	mov al,'*'
	mov ah,0x1
	mov [es:di],ax
	dec cx

OffsetCalBody:
	add si,2
	mov bl,[snakeOffsets+si]		; row
	mov bh,[snakeOffsets+si+1]		; clm
	
	mov al,80
	mul bl
	add al,bh
	shl ax,1
	mov di,ax

	mov al,'*'
	mov ah,0x07
	mov [es:di],ax

	loop OffsetCalBody
	
	pop bx
	pop di
	pop dx
	pop si
	pop cx
	pop es
	pop ax
	ret

;------------------------------------------------------------------------------------------------------

CheckBoundary:
   	push ax
    	push bx
    	push dx

    	mov al, [snakeOffsets]     
    	mov bl, [snakeOffsets+1]  

    	cmp al, 6		; top row
    	jbe putflag
    
    	cmp al, 21		; bottom row
    	jae putflag
    
    	cmp bl, 14		; leftcolumn
    	jbe putflag
    
    	cmp bl, 45		; right column
    	jae putflag

	jmp allgood

putflag:
	mov byte [flag], 1
    
allgood:
    	pop dx
    	pop bx
    	pop ax
    	ret

;-----------------------------------------------------------------------------------------------------------------

CheckSnakeCollision:
    	push ax
    	push bx
    	push cx
    	push dx
    	push si
    	push di

       	mov al, [snakeOffsets]        ; Head row
    	mov bl, [snakeOffsets+1]      ; Head column

    
    	mov si, 2                    
    	mov cx, [snakelength]        

CheckCollisionLoop:
    
    	mov dl, [snakeOffsets+si]    ; Body row
    	mov dh, [snakeOffsets+si+1]  ; Body column

    
    	cmp al, dl                    
    	jne Nocollision               

    	cmp bl, dh                    
    	jne Nocollision               

    	mov byte [flag], 1  
	jmp rettime              

Nocollision:
    	add si, 2                    
    	loop CheckCollisionLoop       

rettime:
       	pop di
    	pop si
    	pop dx
    	pop cx
    	pop bx
    	pop ax

    	ret




;--------------------------------------------------------------------------------------------------------------------

gameOver:
    
	call clrscr

    	mov ax, 17		; Game over msg
    	push ax
    	mov ax, gameOverMsg
    	push ax
    	mov ax, 9
    	push ax
    	mov ax, 31
    	push ax
    	call printstr

	mov ax, 58		; Options msg
    	push ax
    	mov ax, restartOrExitMsg
    	push ax
    	mov ax, 11
    	push ax
    	mov ax, 10
    	push ax
    	call printstr

 
	ret


;-------------------------------------------------------------------------------------------------------

GenerateFood:
    	push ax
    	push bx
    	push cx
    	push dx
    	push es
    	push di

    	mov ax, 0xb800
    	mov es, ax
							; Generating Random Food Row and Column by using Snake Offsets as a parameter because they are changing constantly
       	mov al, [randomSeed]
    	add al, [snakeOffsets]      
    	xor al, [snakeOffsets+1]
    	rol al, 1                   
    	add al, 13                 
    	mov [randomSeed], al        

    	xor ah, ah
    	mov bl, 11                  
    	div bl
    	add ah, 7
    	mov [foodRow], ah

    	
    	mov al, [randomSeed]
    	add al, [foodRow]          
    	xor al, [snakeOffsets]
    	ror al, 1                   
    	add al, 23
    	mov [randomSeed], al        

    	xor ah, ah
    	mov bl, 30
    	div bl
    	add ah, 15
    	mov [foodCol], ah

DrawFood:
    	mov al, [foodRow]
    	xor ah, ah
    	mov bl, 80
    	mul bl
    	mov bx, ax

    	mov al, [foodCol]
    	xor ah, ah
    	add bx, ax
    	shl bx, 1
    	mov di, bx

	mov al, [NormalFoodEaten]
    	cmp al, 4
    	jne NormalFood

    	; Bonus Food
    	mov ah,0x0E
	mov al,'O'     
    	mov [es:di], ax

    	mov byte [isBonusFood], 1
    	mov byte [NormalFoodEaten], 0
    	jmp DoneFood

NormalFood:
    	mov ah, 0x04
	mov al,'o'     
    	mov [es:di], ax
       	mov byte [isBonusFood], 0

DoneFood:
    	pop di
    	pop es
    	pop dx
    	pop cx
    	pop bx
    	pop ax
    	ret


;-------------------------------------------------------------------------------------------------------

CheckFoodCollision:
	push ax
	push bx

	mov al, [snakeOffsets]
	mov bl, [snakeOffsets+1]

	cmp al, [foodRow]
	jne NoCollision
	cmp bl, [foodCol]
	jne NoCollision

	cmp byte [isBonusFood],1
	jne NormalScore
	add word [score],5		; adding 5 points for Bonus Food
	inc byte [BonusCheckForSpeed]
	mov byte [NormalFoodEaten],0
	jmp AfterScoring


NormalScore:
	inc word [score]
	inc byte [NormalFoodEaten]

AfterScoring:
	call GrowSnake
	call printScore
	call GenerateFood

NoCollision:
	pop bx
	pop ax
	ret

;------------------------------------------------------------------------------------------------------

GrowSnake:
	push ax
	push bx
	push cx

	mov cx, [snakelength]
	dec cx
	shl cx, 1
	mov bx,cx


	mov al, [snakeOffsets + bx]
	mov [snakeOffsets + bx + 2], al
	mov al, [snakeOffsets + bx + 1]
	mov [snakeOffsets + bx + 3], al

	inc word [snakelength]

	pop cx
	pop bx
	pop ax
	ret

;-------------------------------------------------------------------------------------------------------

kbisr:
	push ax
	push es

	in al,0x60
	mov ah,al
	
	in al, 0x61
    	or al, 0x80
   	out 0x61, al
    	and al, 0x7F
    	out 0x61, al
	

	mov al, [direction]

	cmp ah,0x48	; up
	jne nextcmp1
	cmp al,'D'
	je nomatch
	mov byte [direction], 'U'
	jmp nomatch

nextcmp1:
	cmp ah,0x50	; down
	jne nextcmp2
	cmp al,'U'
	je nomatch
	mov byte [direction], 'D'
	jmp nomatch

nextcmp2:
	cmp ah,0x4B	; Left
	jne nextcmp3
	cmp al,'R'
	je nomatch
	mov byte [direction], 'L'
	jmp nomatch

nextcmp3:
	cmp ah,0x4D	; right
	jne nomatch
	cmp al,'L'
	je nomatch
	mov byte [direction], 'R'


nomatch:
	mov al,0x20
	out 0x20,al

	pop es
	pop ax

	jmp far[cs:oldkbisr]

	iret

;------------------------------------------------------------------------------------------------------

UpdateOffsets:
	push cx
	push si
	push di
	push bx

    	mov si, [snakelength]
    	dec si               
        shl si,1
	mov cx, [snakelength]
    	dec cx

Shifting:
	sub si,2
	
    	mov al, [snakeOffsets + si]       
    	mov [snakeOffsets + si + 2], al

    	mov al, [snakeOffsets + si + 1]   
    	mov [snakeOffsets + si + 3], al

 
    	loop Shifting

	pop bx
    	pop di
    	pop si
    	pop cx
    	ret

;------------------------------------------------------------------------------------------

UpdateHead:
    	push ax
    	push bx

	mov ax,cs
	mov ds,ax

    	mov al, [direction]        
    	mov bl, [snakeOffsets]     
    	mov bh, [snakeOffsets+1]   

    	cmp byte al, 'U'
    	jnz checkDown
    	dec bl                     ; dec row
    	jmp doneUpdate

checkDown:
    	cmp byte al, 'D'
    	jnz checkLeft
    	inc bl                     ; inc row
    	jmp doneUpdate

checkLeft:
    	cmp byte al, 'L'
    	jnz checkRight
    	dec bh                     ; dec clm
    	jmp doneUpdate

checkRight:
    	cmp byte al, 'R'
    	jnz doneUpdate
    	inc bh                     ; inc clm

doneUpdate:
    	mov [snakeOffsets], bl     ; Update row
    	mov [snakeOffsets+1], bh   ; Update column

    	pop bx
    	pop ax

	call CheckBoundary 

   	ret

;--------------------------------------------------------------------------------------------

EraseTail:
    	push ax
    	push bx
   	push di
    	push es
	push dx

    	mov ax, 0xb800
    	mov es, ax

    	mov bx, [snakelength]
	dec bx
	mov si, bx
	shl si, 1

	mov dl, [snakeOffsets + si]
	mov dh, [snakeOffsets + si + 1]


    	mov al, 80
    	mul dl
    	add al, dh
    	shl ax, 1
    	mov di, ax

    	mov word [es:di], 0x0720   

	pop dx
    	pop es
    	pop di
    	pop bx
    	pop ax
    	ret

;--------------------------------------------------------------------------------------------

MoveSnake:
	call EraseTail
    	call UpdateOffsets
  	call UpdateHead
    	call SnakeDraw
    	ret



;--------------------------------------------------------------------------------------------

tisr:

	cmp byte [flag], 1
	je  rettime2

    	push ax
    	push cx
    	push ds
    
	mov ax,0xb800
	mov es,ax

	mov ax, cs
    	mov ds, ax



    	inc word [tickCount]
	mov ax,[tickCount]

	cmp byte [BonusCheckForSpeed],0
	jz InitialSpeed
	
	cmp byte [BonusCheckForSpeed],1
	jz MediumSpeed

	cmp byte [BonusCheckForSpeed],2
	jz HighSpeed

InitialSpeed:
    	cmp ax,15     			
    	jne skipSnakeMove
	jmp FurthurOperations

MediumSpeed:
	cmp ax,10     			
    	jne skipSnakeMove
	jmp FurthurOperations

HighSpeed:
	cmp ax,5     			
    	jne skipSnakeMove
	jmp FurthurOperations

FurthurOperations:
	call MoveSnake
	call CheckSnakeCollision
	call CheckFoodCollision

    	mov word [tickCount], 0      ; Reset counter

    	               

skipSnakeMove:

    	mov al, 0x20
    	out 0x20, al

    	pop ds
    	pop cx
    	pop ax


    	iret

rettime2:
	mov al, 0x20
    	out 0x20, al
	call gameOver
	iret

;--------------------------------------------------------------------------------------------------------------------

start:
	call clrscr
	mov word [tickCount], 0
	mov byte [direction],'R'

	mov ax,7		; msg print
	push ax
	mov ax,msg1
	push ax
	mov ax,4
	push ax
	mov ax, 60
	push ax
	call printstr

	
	mov ax,27		; msg print
	push ax
	mov ax,msg2
	push ax
	mov ax,1
	push ax
	mov ax, 25
	push ax
	call printstr

	call printsq		; square print

	call printScore		; score print

	call SnakeDraw		; snake draw

	call GenerateFood	; food generation

	xor ax,ax		 
	mov es,ax

	mov ax,[es:9*4]		; storing original keyboard interrput offset and segement
	mov [oldkbisr],ax
	mov ax,[es:9*4+2]
	mov [oldkbisr+2],ax

	mov ax,[es:8*4]		; storing original keyboard interrput offset and segement
	mov [oldtisr],ax
	mov ax,[es:8*4+2]
	mov [oldtisr+2],ax


	cli
	mov word [es:9*4],kbisr	; hooking keyboard interrupt
	mov [es:9*4+2],cs
	mov word [es:8*4], tisr	; hooking timer interrupt 
	mov [es:8*4+2], cs
	sti

final:

	
	mov ah,0x0	; serive 0 - get keystroke
	int 0x16	; BIOS keyboard serive

	
	mov bl,al

	cmp bl, 13	; Enter key 
	je Restart

	cmp bl, 32	; SpaceBar key
	je terminate

	jmp final
	

Restart:

	cli
	xor ax, ax
	mov es, ax

	mov ax, [oldtisr]		; Unhooking timer interrupt
	mov [es:8*4], ax
	mov ax, [oldtisr+2]
	mov [es:8*4+2], ax

	mov ax, [oldkbisr]		; Unhooking keyboard interrupt
	mov [es:9*4], ax
	mov ax, [oldkbisr+2]
	mov [es:9*4+2], ax

	sti

	mov byte [flag], 0		; setting initial flags
	mov word [score], 0
    	mov word [snakelength], 3
    	mov byte [direction], 'R'
    	mov word [tickCount], 0
	mov byte [BonusCheckForSpeed], 0
	mov byte [NormalFoodEaten], 0
	mov byte [isBonusFood], 0
    
    					; Reset snake positions

    	mov byte [snakeOffsets+0], 13
    	mov byte [snakeOffsets+1], 31
    	mov byte [snakeOffsets+2], 13
    	mov byte [snakeOffsets+3], 30
    	mov byte [snakeOffsets+4], 13
    	mov byte [snakeOffsets+5], 29

	jmp start

terminate:
	cli

	xor ax, ax			; Unhooking timer interrupt
	mov es, ax
	mov ax, [oldtisr]
	mov [es:8*4], ax
	mov ax, [oldtisr+2]
	mov [es:8*4+2], ax

	mov ax, [oldkbisr]		; Unhooking keyboard interrupt
	mov [es:9*4], ax
	mov ax, [oldkbisr+2]
	mov [es:9*4+2], ax

	sti

	call clrscr

	mov ax, 38		; Options msg
    	push ax
    	mov ax, endingMsg
    	push ax
    	mov ax, 11
    	push ax
    	mov ax, 21
    	push ax
    	call printstr

	mov ax,0x4C00
	int 0x21



; variables

flag: db 0			; Checks that if snake has collided with wall or with itself
BonusCheckForSpeed: db 0	; After every bonus food, speed will increase a little
NormalFoodEaten db 0   		; count for normal food eaten
isBonusFood: db 0      		; flag if the snake has eaten bonus food
randomSeed: db 7  		; For random food generation
oldkbisr: dd 0
oldtisr: dd 0
score: dw 0
snakelength: dw 3		; Initially my snake length is 3
direction: db 'R'
tickCount: dw 0
foodRow: db 0
foodCol: db 0
snakeOffsets: db 13,31,13,30,13,29 	;(row, clm) of 3 offsets  Initially my snake length is 3
