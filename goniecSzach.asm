;===================================================
;===================================================
;=============== MAKRA           ===================
;===================================================
;===================================================

prints macro string     ;print String
pusha
    mov ax,dane1
    mov ds,ax
    mov dx,offset string
    mov ah,9
    int 21h
popa
endm

endl MACRO               ;PrzejdŸ do nowej Linii
pusha
    mov ah, 2
    mov dl, 0Dh          ; CR = carriage return
    int 21h
    mov dl, 0Ah          ; LF = line feed
    int 21h
popa
ENDM


print MACRO value ;Funkcja wypisujaca zawartoœæ 2 bajtów 
                  ;Czyli np. je¿eli wykonamy "print 2233h" to OUTPUT:2233
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



break MACRO  ;zatrzymaj wykonywanie programu 
pusha
    mov ah,01h
    int 21h
popa
ENDM

nr MACRO x, y ;zwraca w bx numer pola tablicy odpowiadaj¹cego wspó³rzêdnym (x,y)
push dx       ;
push bx
push ax

    mov dh,x
    mov dl,y

    xor ax,ax
    xor bx,bx

    mov al,dl
    mov bl, 17
    mul bl     ;ax=17*y

    mov bl,al
    add bl,dh

pop ax
pop dx
pop dx
ENDM


;===================================================
;===================================================
;=============== G£ÓWNA CZÊŒÆ    ===================
;===================================================
;===================================================
assume    cs:code1
.186

dane1     segment
ascii db ' ','.', 'o', '+', '=', '*', 'B', 'O', 'X', '@', '%', '&', '#', '/', '^'  ; tablica znakow ascii
errlength db "blad dlugosci$"
errsyntax db "blad skladni$"
space db " $"
ramkaD db "+-----------------+$"
ramkaG db "+--[ RSA 1024]----+$"
dlugosc db ?
buff db 127 dup ('$')
argoff dw 127 dup ('$$')
arglen db 127 dup ('$')
array db 16 dup (0)
tablica db 153 dup (0)
bishop db 8,4  ;Aktualne polozenie gonca

dane1    ends

code1    segment

start1:    mov    ax,seg top1
mov    ss,ax
mov    sp,offset top1




mov ax,dane1
mov es,ax

call parser                  ;umieszcza w buff wszystkie znaki niebêd¹ce spacj¹, w argoff umieszcza offsety kolejnych argumentów, a w arglen d³ugoœæ argumentów
;call dlugosc_buff
call check_buffor            ;sprawdza poprawnoœæ danych, umieszcza 16 bajtów w array
;call wypisz_buff
;call wypisz_array
call modification            ;negacja bajtów w array
call fill_tablica            ;wykonaj ruchy w z array i umieœæ w polu tablicy liczbê odwiedzin przez goñca
call convert_tablica         ;zmieñ wartoœci tablicy na 14 znaków ascii


mov ah,es:[bishop]           ;ustaw 'S' i 'E' 
mov al,es:[bishop+1]
nr ah,al
mov es:[tablica+bx],'E'
nr 8,4
mov es:[tablica+bx],'S'

call drukuj                  ;wypisz tablicê


finish:
    mov ah,4ch
    int 21h


;===================================================
;===================================================
;===========   PROCEDURY       =====================
;===================================================
;===================================================

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
	je errorn
		
	
	

    while1:
        mov ah,ds:[81h+si]
        cmp ah,' '  ;PSP: is char a space
        je end1
            mov al,ds:[81h+ si -1]
            cmp al,' ';PSP: is previous char a space
            jne save_to_buff 
                new_arg: ; <space> <char>
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
	prints errlength
	jmp finish

parser endp
;==============

dlugosc_buff proc
pusha
    mov bx,0
    mov ax,0
    xor dx,dx
    petla:
        mov dl,es:[arglen+bx]
        cmp dl,'$'
        je przerwij
        add ax,dx
        inc bx
        jmp petla
    przerwij:
    mov es:[dlugosc],al
popa
ret
dlugosc_buff endp


;=============
check_buffor proc
pusha
    mov ax,dane1
    mov es,ax
    
    mov cl,es:[arglen] ;sprawdz czy pierwszy argument tj. '0' lub '1' jest poprawny
    cmp cl,1
    jne error0
    mov si,es:[argoff] ;zaladuj do SI offset pierwszego argumentu buffora
    mov dl,es:[si]
    cmp dl,'0'
    jb error1
    cmp dl,'1'
    ja error1

    mov si,es:[argoff+2] ; zaladuj do SI offset drugiego argumentu buffora

    xor bx,bx
    xor cx,cx
    xor di,di            ;iterator po array
    mov cl,es:[arglen+1] ; zaladuj do CX dlugosc drugiego argumentu
    cmp cx,32            ; Czy dlugosc drugiego argumentu wynosi 32?
    jne error0

    check_arg:
        mov dl,es:[si+bx]    ;zaladuj do DL znak o indeksie BX drugiego argumentu

        range:
        cmp dl,'0'
        jb error1

        cmp dl,'9'
        jbe decimal

        cmp dl,'A'
        jb error1

        cmp dl,'F'
        jbe caseupper

        cmp dl,'a'
        jb error1

        cmp dl,'f'
        jbe caselower

        jmp error1

    decimal:
        sub dl,30h
        jmp done
    caselower:
        sub dl,87
        jmp done
    caseupper:
        sub dl,55
        jmp done

    done: ;AX,BH s¹ nieu¿ywane wy¿ej wiêc mogê je tu u¿yæ
        xor ax,ax  ;sprawdzam reszte z dzielenia DL przez 2
        mov al,bl
        mov dh,2
        div dh
        cmp ah,0
        je parzysta
        cmp ah,1
        je nieparzysta

    parzysta: ;sprytnie umieszcza w array ¿eby z dwóch bajtów np. '0F' ,'0C' zrobiæ 'FC'
        shl dl,4
        mov es:[array+di],dl
        jmp wstawiona
    nieparzysta:
        add es:[array+di],dl
        inc di
        jmp wstawiona

        wstawiona:
            inc bx
    loop check_arg
    popa
    ret

    error0:
        prints errlength
        jmp finish
    error1:
        prints errsyntax
        jmp finish

    check_buffor endp
    ;==================


wypisz_buff proc
pusha
    xor cx,cx
    mov cl,es:[dlugosc]
    xor dx,dx
    mov bx,0
    wf:
        mov dl,es:[buff+bx]
        print dx
        prints space
        inc bx
    loop wf
popa
ret
wypisz_buff endp

;=============

wypisz_array proc
pusha
    xor cx,cx
    mov cl,16
    xor bx,bx
    xor dx,dx
    dotrzydziestudwoch:
        mov dl,es:[array+bx]
        print dx
        prints space
        inc bx
    loop dotrzydziestudwoch
popa
ret
wypisz_array endp

;=================
modification proc
    pusha
    mov dl,es:[buff]
    cmp dl,'0'
    je wykonane
    
    xor bx,bx 
    mov cx,16
    wykonujNaTablicyArray:
        mov ah,es:[array+bx]
        not ah
        mov es:[array+bx],ah 
        inc bx
    loop wykonujNaTablicyArray
    
    wykonane:
        popa
        ret
modification endp


;==============================

fill_tablica proc
pusha

    xor cx,cx
    mov cl,16
    mov bx,0       ;BX - który rozwa¿am bajt w array

    execute:
    mov ah,es:[array+bx]  ;AH - ca³y bajt ,AL - dwa ostatnie bity tego bajtu

    push cx
        mov cx,4
        cztery_shr:    
            mov al,ah
            and al,0011b  ; umiesc w AL dwa ostatnie bity bajtu
            w1:
                cmp al,00b
                jne w2
                call up_left
            
            w2:
                cmp al,01b
                jne w3
                call up_right
            w3:
                cmp al,10b
                jne w4
                call down_left
            w4:
                cmp al,11b
                jne dalej1
                call down_right
            
            dalej1:
                shr ah,2     ;przesun bajt o dwa miejsca w prawo
        loop cztery_shr
    pop cx

    inc bx
    loop execute
popa
ret
fill_tablica endp
;===========================================================================================
up_left proc
pusha
    mov ah,es:[bishop]      ;x - biskupa
    mov al,es:[bishop+1]    ;y - biskupa

    cmp ah,0
    jne nextA
        cmp al,0
        je doneA
    nextA:

    cmp al,0
    je up_edgeA            ; dolna krawedz

    cmp ah,0               ;Czy x=0, y<{0,1,2,3,4,5,6,7,8}
    jne normalA           
    mov cl,0
    czykrawedzbocznaA:
        cmp al,cl
        je left_edgeA
        add cl,1
        cmp cl,8
        ja normalA
    jmp czykrawedzbocznaA

        normalA:
            dec ah
            dec al
            jmp doneA

        up_edgeA:
            dec ah
            jmp doneA
        left_edgeA:
            dec al
            jmp doneA

            doneA:
                nr ah, al
                inc es:[tablica+bx]
                mov es:[bishop],ah      ;x - biskupa  ZAPISZ BIERZACE POLOZENIE BISKUPA
                mov es:[bishop+1],al    ;y - biskupa
popa
ret
up_left endp
;==================;

up_right proc
pusha
    mov ah,es:[bishop]      ;x - biskupa
    mov al,es:[bishop+1]    ;y - biskupa

    cmp ah,16
    jne nextB
        cmp al,0
        je doneB
    nextB:
    
    cmp al,0
    je up_edgeB            ; gorna krawedz

    cmp ah,16                 ;Czy x=16, y<{0,1,2,3,4,5,6,7,8}
    jne normalB
    mov cl,0
    czykrawedzbocznaB:            ;prawa krawedz
        cmp al,cl
        je right_edgeB
        add cl,1
        cmp cl,8
        ja normalB
    jmp czykrawedzbocznaB


    normalB:
        inc ah
        dec al
        jmp doneB

    up_edgeB:
        inc ah
        jmp doneB
    right_edgeB:
        dec al
        jmp doneB

    doneB:
        nr ah, al
        inc es:[tablica+bx]
        mov es:[bishop],ah      ;x - biskupa  ZAPISZ BIERZACE POLOZENIE BISKUPA
        mov es:[bishop+1],al    ;y - biskupa
popa
ret
up_right  endp

;=========================================


down_left proc
pusha
    mov ah,es:[bishop]      ;x - biskupa
    mov al,es:[bishop+1]    ;y - biskupa

    cmp ah,0
    jne nextC
        cmp al,8
        je doneC
    nextC:

    cmp al,8
    je down_edgeC            ; dolna krawedz

    cmp ah,0                     ;Czy x=0, y<{0,1,2,3,4,5,6,7,8}
    jne normalC
    mov cl,0
    czykrawedzbocznaC:            ;lewa krawedz
        cmp al,cl
        je left_edgeC
        add cl,1
        cmp cl,8
        ja normalC
    jmp czykrawedzbocznaC

    normalC:
        dec ah
        inc al
        jmp doneC

    down_edgeC:
        dec ah
        jmp doneC
    left_edgeC:
        inc al
        jmp doneC

    doneC:
        nr ah, al
        inc es:[tablica+bx]
        mov es:[bishop],ah      ;x - biskupa  ZAPISZ BIERZACE POLOZENIE BISKUPA
        mov es:[bishop+1],al    ;y - biskupa
popa
ret
down_left  endp

;=================

down_right proc
pusha
    mov ah,es:[bishop]      ;x - biskupa
    mov al,es:[bishop+1]    ;y - biskupa

    cmp ah,16
    jne nextD
        cmp al,8
        je doneD
    nextD:

    cmp al,8
    je down_edgeD            ; dolna krawedz

    cmp ah,16                   ;Czy x=16, y<{0,1,2,3,4,5,6,7,8}
    jne normalD
    mov cl,0
    czykrawedzbocznaD:            ;prawa krawedz
        cmp al,cl
        je right_edgeD
        add cl,1
        cmp cl,8
        ja normalD
    jmp czykrawedzbocznaD

    normalD:
        inc ah
        inc al
        jmp doneD

    down_edgeD:
        inc ah
        jmp doneD
    right_edgeD:
        inc al
        jmp doneD

    doneD:
        nr ah, al
        inc es:[tablica+bx]
        mov es:[bishop],ah      ;x - biskupa  ZAPISZ BIERZACE POLOZENIE BISKUPA
        mov es:[bishop+1],al    ;y - biskupa
popa
ret
down_right  endp


;==================================================================

convert_tablica proc
    pusha
   
    mov bx,0
    petla20:
        xor ax,ax
        mov al,es:[tablica+bx]
        mov di,ax
        cmp al,14
        jb next20
            mov di,13
        next20:
        
        mov al,es:[ascii+di]
        mov es:[tablica+bx],al
        inc bx
        cmp bx, 152
        jbe petla20  
        
     
    popa
    ret
    
convert_tablica endp

;=====================================================================================
drukuj proc
pusha
    prints ramkaG   ;wypisz gorna ramke tablicy
    endl

    xor bx,bx
    dowhile:
        
        mov al,bl  ;sprawdza czy BX przystaje do 0 mod 17, jeœli tak to pisze ramkê
        xor ah,ah
        mov dl,17  ;dzielnik=17
        div dl
        cmp ah,0
        jne dalej
        mov ah,2     ;wypisz lewy znak ramki tablicy
        mov dl,'|'
        int 21h

        dalej:

        push ax                  ;pisze bx-owy element tablicy
            xor dx,dx
            mov dl,es:[tablica+bx]
            mov ah,2
            int 21h
        pop ax

        xor ax,ax  ;sprawdza czy BX przystaje do 16 mod 17 jeœli tak to pisze praw¹ ramkê
        mov al,bl  ;wykorzystujê resztê z dzielenia czyli modulo
        mov dl,17  ;dzielnik=17
        div dl
        cmp ah,16
        jne kontynuuj
        mov ah,2     ;wypisz prawy znak ramki tablicy
        mov dl,'|'
        int 21h
        endl         ;idz do nowej linii

        kontynuuj:
            cmp bx,152
            je zrobione
        inc bx
    jmp dowhile

    zrobione:
        prints ramkaD   ;wypisz dolna ramke tablicy
popa
ret
drukuj endp

;===================================================
;===================================================
;=========== KONIEC PROCEDUR            ============
;===================================================
;===================================================
code1    ends


stos1    segment stack
dw    200 dup(?)
top1    dw    ?
stos1    ends

end start1

