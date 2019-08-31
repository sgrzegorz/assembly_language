;=============================================MAKRA============================================================================================================;
;----------makro wypisujace string ------------------------------;
prints macro string     ;print String
pusha
    mov ax,dane1
    mov ds,ax
    mov dx,offset string
    mov ah,9
    int 21h
popa
endm
;--------makro powodujace przejscie do nowej linii---------------;
endl MACRO               ;Przejdü do nowej Linii
pusha
    mov ah, 2
    mov dl, 0Dh          ; CR = carriage return
    int 21h
    mov dl, 0Ah          ; LF = line feed
    int 21h
popa
ENDM

;---------makro wypisujace zawartosc 2 bajtow --------------------;
print MACRO value 
                  ;Czyli np. jeøeli wykonamy "print 2233h" to OUTPUT:2233
LOCAL analyse, write, show
pusha

    mov ax, value

    xor cx, cx          ; repetitions
    mov bx, 16          ; divisor

    analyse:        ; First step: push the digits into stack
        xor dx, dx      ; Clear DX for division
        div bx          ; DX:AX/BX = AX remainder DX
        push dx
        inc cx
        cmp ax, 0       ; break condition
    jne analyse

    write:          ; Second step: pop the digits from stack and print
        pop dx          ; Only DL is needed
        add dl, "0"
        cmp dl, "9"     ; Is DL digit?
        jbe SHORT show  ; Yes: skip the next line
        add dl, 7       ; Adjust ASCII
    show:
        mov ah, 2       ; Print character to STDOUT
        int 21h
    loop write

popa
ENDM


;------------makro zatrzymujace dzialanie programu ------------;
break MACRO   
pusha
    mov ah,01h
    int 21h
popa
ENDM



;=======================================================GLOWNA CZESC==========================================================================================;


;-------------------------------------------------------segment danych--------------------------------------------------------------------------------------;

dane1     segment
;---parser---;
buff db 127 dup ('$')
argoff dw 127 dup ('$$')
arglen db 127 dup ('$')
iloscArg dw 0        ;zmienna wykorzystywana zeby sprawdzic czy ilosc argumentow jest poprawna

;---convertToFloat---;
cyfryPoPrzecinku dw 0
calkowita dw ?
ulamkowa dw ?
minus db 0

;---FPU---;
xmin dq ? 
xmax dq ?
ymin dq ?
ymax dq ?
p dq ?
q dq ?
value dq 4.0
res_x dw 320d        ;rozdzielczosc pozioma ekranu
res_y dw 200d        ;rozdzielczosc pionowa ekranu

;----tryb graficzny---;
x dw 0               ;wspolrzedna x, N z tresci zadania
y dw 0               ;wspolrzedna y, M z tresci zadania


errnumber db "Nieporawna ilosc argumentow.$"
errsyntax db "Blad skladni.$"
dziesiec dw 10d           ;liczba 10 potrzebna do obliczen

dane1    ends


assume cs:code1, ss:stos1, ds:dane1


;-------------------------------------------------------------segment stosu------------------------------------------------------------------------------------;

stos1    segment stack  
dw    400 dup(?)
top1    dw    ?
stos1    ends

;-------------------------------------------------------------segment kodu-------------------------------------------------------------------------------------;
code1    segment
.386		      ;dyrektywa odpowiedzialna min za pusha, popa	
.387              ;dyrektywa wlaczajaca obsluge kooprocesora


start1:    mov    ax,seg top1
mov    ss,ax
mov    sp,offset top1

call parser

mov ax,dane1
mov ds,ax

;---------sprawdz czy liczba argumentow wynosi 4 --------;
cmp [iloscArg],4
je kontynuujem
 prints errnumber
  jmp finish
kontynuujem:
finit 


;--------funkcje wyliczajace xmin xmax ymin ymax na podstawie parsera---;
;bx nr argumentu
;di adres gdzie chce zapisac
mov di,offset xmin
mov bx,0d
call convertToFloat  

mov di,offset xmax
mov bx,1d
call convertToFloat

mov di,offset ymin
mov bx,2d
call convertToFloat

mov di,offset ymax
mov bx,3d
call convertToFloat

call rysujMandelbrota

	
	
finish:
	xor	ax,ax
	int	16h  ;czekaj na dowolny klawisz

	mov	al,3  ; tryb tekstowy 80x20 znakow
	mov	ah,0  ;zmiana trybu vga
	int	10h

    mov ah,4ch
    int 21h


;=============================================================PROCEDURY=========================================================================================;


;--------------------------procedura zmieniajaca arguement parsera na liczbe float------------------------------------------------------------------------------;
; bx -nr argumentu z parsera
; di miejsce gdzie chcemy zapisac obliczonego float'a
convertToFloat proc
pusha
    push bx
	add bx,bx
    mov si,[argoff+bx] ;adres liczby zmiennoprzecinkowej
	
	pop bx
	xor cx,cx           ; CL - ile juz znakow argumentu przetworzono
	mov ch,[arglen+bx]  ;liczba bajtow ktora ma argument numer bx
	
	cmp ch,3d            ;Czy liczba zmiennoprzecinkowa ma conajmniej dlugosc 3. "1.2"
	jb error0
	
    xor ax,ax            ;tutaj bedzie liczba w postaci dziesietnej
	xor dx,dx
	mov dl,byte ptr [si] ;aktualnie przetwarzany bajt

	cmp dl,"-"          ;czy liczba jest ujemna?
	jne liczbaJestDodatnia
		inc cl
		inc si
		mov dl,byte ptr [si]
		inc [minus]
			
	liczbaJestDodatnia:
		cmp dl,"." ;czy liczba zmiennoprzecinkowa ma format ".124" lub "-.1234"?
	    je error0
	
	cmp dl,"0" ;musi byc liczba przed kropka
	jb error0
	cmp dl,"9"
	ja error0
	
	
	;------------------czesc calkowita ------------------------;
	petlaCalkowita:
	    
	    cmp cl,ch
		jae error0        ; Czy liczba zmiennoprzecinkowa ma format:  "1234"?
		
		mov dl,byte ptr [si]  ;umiesc w dl nasza cyfra
		
		cmp dl, "."
		je break0
		
		cmp dl,"0"    
		jb error0
		cmp dl,"9"
		ja error0
		sub dl,"0"     ;zamien dl na cyfre
		
		push dx         ;podziel rejestr ax (w ktorym bedzie przechowywana konwertowana czesc calkowita przez 10
		mul  [dziesiec] ;uwaga ulega zmianie dx!
		pop dx
		add ax,dx       ;dodaj do ax, dl aktualnie przetwarzany bajt
		inc cl          ;zwieksz ilosc bajtow juz przetworzonych
		inc si          ;przesun wskaznik na kolejny bajt argumentu 
	jmp petlaCalkowita
	
	
	break0:                 ;dl="." cl jest przed kropka
		mov [calkowita],ax  ;zapisz uzyskana w petliCalkowita czesc calkowita liczby do segmentu danych
		
	    inc si  ;minalem kropke
		mov dl,byte ptr [si]
	    inc cl  ;przetworzylem kropke
	    
	    cmp cl,ch   ;Czy liczba zmiennoprzecinkowa ma format: "12344."?
		je error0   
		cmp dl,"0" ;czy po kropce jest cyfra?
		jb error0
		cmp dl,"9"
		ja error0
		
	;------------------czesc ulamkowa ----------------------------;
	xor ax,ax   ;przechowuje tu czesc ulamkowa	
	petlaUlamkowa:
		cmp cl,ch  ;Czy dotarto juz do konca liczby zmiennoprzecinkowej?
		je break1
		cmp dl,"0"
		jb error0
		cmp dl,"9"
		ja error0
		sub dl,"0"
		push dx
			mul [dziesiec] ;pomnoz czesc ulamkowa przez 10 aby nastepnie dodac do niej dl - aktualnie przetwarzany bajt dl - zamnieniony na liczbe
		pop dx
		add ax,dx
		inc cl            ;zwieksz ilosc przetworzonych bajtow
		inc si            ;przesun wskaznik na aktualnie przetwarzany bajt
		mov dl,byte ptr [si]   ;zaladuj aktualnie przetwarzany bajt
		inc [cyfryPoPrzecinku]    ;ilosc cyfr czesci ulamkowej, uzyte za kilka linijek w FPU
	jmp petlaUlamkowa
	

	
	break1:
		mov [ulamkowa],ax    ;zapisz czesc ulamkowa uzyskana w petlaUlamkowa do segmentu danych
		
		fild word ptr [calkowita] ;st(2) ;podziel czesc ulamkowa przez 10*cyfryPoPrzecinku i dodaj do niej czesc calkowita
		fild word ptr [ulamkowa]  ;st(1)
		fild word ptr [dziesiec]  ;st(0)
		mov cx,[cyfryPoPrzecinku]
		petla3:
			fdiv st(1),st(0)
		loop petla3
		fsubp st(0),st(0)
		faddp st(1),st(0)
		cmp byte ptr [minus],0
		je kontynuuj
			fchs  ;zmien znak na przeciwny
		kontynuuj:
		fstp qword ptr [di]   ;zapisz przekonwertowana liczba zmiennoprzecinkowa do pamieci
		
		mov word ptr [minus], 0          ;wyzeruj uzyte przez ta funkcje dane z pamieci
		mov word ptr [cyfryPoPrzecinku],0
		mov word ptr [calkowita],0
		mov word ptr [ulamkowa],0
popa
ret
    
	error0:
		prints errsyntax
		jmp finish
	
convertToFloat endp


;-------------------------------------funkcja parsujaca argumenty--------------------------------------------------------------------------------------------;

parser proc
pusha

    mov ax,dane1
    mov es,ax

    mov si,0 ;PSP paramterer iterator SI
    mov di,0 ;buff iterator   DI
    mov bx,0 ;arglen iterator
    mov dx,0 ;argoff iterator

    xor cx,cx
    mov cl, ds:[80h] ;i=
    cmp cl,0
	je errorn  ;jezeli nie ma zadnych argumentow wypisz blad
		
	
	

    while1:
        mov ah,ds:[81h+si]
        cmp ah,' '  ;PSP: is char a space
        je end1
            mov al,ds:[81h+ si -1]
            cmp al,' ';PSP: is previous char a space
            jne save_to_buff 
                new_arg: ; <space> <char>
				    inc es:[iloscArg]
                    inc bx 
                    mov es:[arglen+bx-1],1h
                    add dx,2
                    pusha
                        mov bx,dx
                        mov si,offset buff
                        add si,di
                        mov es:[argoff+bx-2],si
                    popa
                    mov es:[buff+di],ah ;save PSP char to buff
                    inc di   ;inc buff iterator
                    jmp end1

            save_to_buff:
                mov es:[buff+di],ah ;save PSP char to buff
                inc di   ;inc buff iterator
                inc es:[arglen+bx-1] ;inc the length of current arg
                jmp end1

        end1:
            inc si ;inc PSP iterator
    loop while1
popa
ret

errorn:
	prints errnumber
	jmp finish

parser endp



;--------------------------------------------------------------------------------------------------------------------------------------------------------------;
wyliczP proc
    pusha
	
	     
	fld qword ptr [xmin]
	fild word ptr [res_x]
	fild word ptr [x]
	fld qword ptr [xmax]
	fld qword ptr [xmin]
	fsubp st(1),st(0)    
	fmulp st(1),st(0)   
	fxch st(1)     ;podzielic w dobrej kolejnosci
    fdivp st(1), st(0)	 
	faddp st(1),st(0)    
	fstp qword ptr [p]

	
	popa
ret
wyliczP endp

;--------------------------------------------------------------------------------------------------------------------------------------------------------------;

wyliczQ proc
    pusha
	
	fld qword ptr [ymin]
	fild word ptr [res_y]
	fild word ptr [y]
	fld qword ptr [ymax]
	fld qword ptr [ymin]
	fsubp st(1),st(0)    
	fmulp st(1),st(0)    
	fxch st(1)             ;podzielic w dobrej kolejnosci
    fdivp st(1), st(0)	 
	faddp st(1),st(0)     
	fstp qword ptr [q]
	popa
	ret
wyliczQ endp

;------------------------------------funkcja liczaca jaki kolor nalezy wpisac do piksela----------------------------------------------------------------------;

jakiKolor proc  ;zwraca w al wartosci 1-bialy lub 0-czarny
    push cx
mov cx,1000d
	
    fld qword ptr [q]    ;st(7)  q
	fld qword ptr [p]    ;st(6)  p
	fldz                 ;st(5)  y
	fldz                 ;st(4)  x
	fldz                 ;st(3)  tmp
	fldz                 ;st(2)  y^2
	fldz                 ;st(1)  x^2
	fldz                 ;st(0)

petla:
    ;tmp
	fsub st(0),st(0) 
	fadd st(0),st(1) ;st(0)=x^2
	fsub st(0),st(2) ;st(0)=x^2-y^2
	fadd st(0),st(6) ;st(0)=x^2-y^2+p
	fxch st(3)       ;st(3)=x^2-y^2+p
	fsub st(0),st(0)
	;y
	fadd st(0),st(4) ;st(0)=x
	fmul st(0),st(5) ;st(0)=x*y
	fadd st(0),st(0) ;st(0)=2*x*y
	fadd st(0),st(7) ;st(0)=2*x*y + q
	fxch st(5)       ;st(5)=2*x*y + q
	fsub st(0),st(0)
	;x
	fxch st(3)       ;st(0)=tmp
	fxch st(4)       ;st(4)=tmp
	fsub st(0),st(0)
	;x^2 
	fadd st(0),st(4) ;st(0)=x
	fmul st(0),st(0) ;st(0)=x^2
	fxch st(1)       ;st(1)=x^2
	fsub st(0),st(0)
	;y^2
	fadd st(0),st(5)  ;st(0)=y
	fmul st(0),st(0)  ;st(0)=y^2
	fxch st(2)        ;st(2)=y^2
	fsub st(0),st(0)
	;x^2+y^2
	fadd st(0),st(1) ;st(0)=x^2
	fadd st(0),st(2) ;st(0)=x^2+y^2
	fcom [value]
	fstsw ax
	sahf
	ja przerwij
loop petla
;------petla skonczyla sie sama----;
	xor ax,ax
	mov al,1
	jmp dalej1
przerwij: ;----petla zostala przerwana-----;
	xor ax,ax
	
	dalej1:
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
			ffree st(0)
	        ;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
			;fsubp st(0), st(0)
    pop cx
ret
jakiKolor endp
;---------------------------------------------------------------------------------------------------------------------------------------------------------------;

rysujMandelbrota proc
pusha 
	mov	al,13h  ; tryb graficzny 320x200, 256 kol
	mov	ah,0  ;zmiana trybu vga
	int	10h

mov cx,200d
petla1:
	push cx
	
		mov cx,320d
		petla2:
			call wyliczP
			call wyliczQ
			call jakiKolor
			cmp al,1
			
			je bialy
				czarny:
					mov bl,00h  ;bl - kolor czarny
				jmp dalej
	            bialy:
					mov bl,0fh ;bl - kolor bialy
	
				dalej:
				  
				    pusha
					mov ax,0a000h ;segment pamieci VGA
					mov es,ax
					mov ax,word ptr [y] ;ax=y
					mov bp,320d         ;bp=320
					mul bp              ;ax=320*y   
					mov si,word ptr [x]     
					add si,ax           ;si=320*y+x
					mov byte ptr es:[si],bl ;wstaw w wyliczone miejsce kolor zapamietany w rejestrze bl
					popa
			           
			inc word ptr [x]  ;zwieksz x
	    loop petla2
		mov word ptr [x],0   ;zeruj x
	
	pop cx
	inc word ptr [y]   ;zwieksz y
loop petla1
popa
ret
rysujMandelbrota endp	

;=========================================================KONIEC PROCEDUR ====================================================================================;
code1    ends

end start1
