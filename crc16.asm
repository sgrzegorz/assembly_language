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

printe macro string     ;print String
pusha
    mov ax,dane1
    mov ds,ax
    mov dx,offset string
    mov ah,9
    int 21h 
    
popa 
    jmp finish
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



;=================================================================================================
;===================================================================================================
;=============== G£ÓWNA CZÊŒÆ    ==================================================================  =
;=================================================== =============================================
;=================================================================================================
assume    cs:code1
.186

dane1     segment          
    
ARGAMOUNT db ?
BUFFLENGTH db ?
BUFF db 127 dup (?)
ARGOFF dw 127 dup ('$$')
ARGLEN db 127 dup (?)             

SPACE db " $"

TEKST db 1000 dup ('$')
VERSION DB ?          ;1 = INPUT OUTPUT ; 2 = -v INPUT1 INPUT2
GETSIZE dw 50d
;input db "input.txt",0 
;output db "output.txt",0 
OUTPUT db 127 dup (?)
INPUT db 127 dup (?) 
;input1 db "input.txt",0
;input2 db "output.txt",0
INPUT1 db 127 dup (?)
INPUT2 db 127 dup (?) 

handle1 dw ?
handle2 dw ?

MESSAGE db "Is ok :) $"
ERROR1 db "Incorrent number of arguments.$"
ERROR2 db "Operator should be -v.$"
ERROR3 db "Cannot open a file.$"     
ERROR4 db "Unexpected error. You are lost.Nic Ci juz nie pomoze...$"  
ERROR5 db "Problem with input2 file.$"  
ERROR6 db "Expected .txt format$"
COMMUNICATE db "Wykryto blad CRC w linii: $"   
LINE dw ?                       
                                              
POLYNOMIAL dw 0A001h
CRC dw ?                                        ;CRC w formie liczby
CRCSTRING db 4 dup('$')                         ;CRC w formie stringu
CRCSTRINGEND db 0Dh, 0Ah, 0                      
CRCIMPORTED db 7 dup('$')                       ;w wersji "-v" znajduje sie w nim CRC aktualnej linii pobrany z input2.txt 
eof db ?                                        ;czy w aktualnym przeladowaniu bufora konczy sie plik input.txt /input1.txt
q dw ?                                          ;wskaznik po probranym tekscie z pierwszego pliku
tekst_size db 50d                               ;ile znakow ma pobrac getchar
tekst_length dw ?                               ;ile znakow tak naprawde pobral getchar/   tekst_size!=tekst_length dla ostatniego wywolania getchar

CRCTABLE    dw 00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
            dw 0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
            dw 0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
            dw 00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
            dw 0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
            dw 01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
            dw 01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
            dw 0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
            dw 0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
            dw 03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
            dw 03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
            dw 0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
            dw 02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
            dw 0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
            dw 0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
            dw 02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
            dw 0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
            dw 06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
            dw 06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
            dw 0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
            dw 07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
            dw 0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
            dw 0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
            dw 07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
            dw 05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
            dw 09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
            dw 09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
            dw 05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
            dw 08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
            dw 04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
            dw 04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
            dw 08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h

dane1    ends                               
                 
                 
code1    segment

start1:    mov    ax,seg top1
mov    ss,ax
mov    sp,offset top1

call parser  

mov ax,dane1
mov ds,ax
call checkArg  
call generateTable                       ;wypelnij tablice CRCTABLE
    
    cmp ds:[VERSION],2                   ;zdecyduj ktory plik otworzyc zaleznie do VERSION =1 lub VERSION =2
    je version2
    version1:
         call open1
         jmp filesOpened   
    version2: 
         call open2 

filesOpened:


 
call getchar                             ;zaladuj do bufora o nazwie TEKST tekst_size znakow z pierwszego pliku


while2:
    mov si,ds:[q]  
    cmp si,ds:[tekst_length] 
    jb czyCRET
       cmp ds:[eof],1 
        jne case1 ;przeladuj buffor
        jmp case2 ;to byla ostatnia linijka
        
            
    czyCRET:
        cmp ds:[TEKST+si],0Dh
        je case4 ;znak CRET nalezy go pominac
            cmp ds:[TEKST+si],0Ah
            je case5  ; NEWL zapisz CRC do pliku
            jmp case3 ; zmien bierzace CRC
    
    case1:; koniec bierzacego getchar, przeladuj buffor
        call getchar
        mov ds:[q],0
        jmp while2  
    
    case2:  ;koniec ostatniej linijki pliku INPUT 
         call crc2string
         cmp ds:[VERSION],2
         je version2Case2
                   version1Case1:
                           
                            call putchar                                    ;Wywolac porownaj =>getcrc/error
                            jmp endOfWhile
                           
                   version2Case2: 
                            call getcrc
                            call compareCRC
                            jmp endOfWhile
                            
    
    case3:  ;zwykly znak
		 xor ax,ax
         mov al,ds:[TEKST+si]
        call crc16
        inc ds:[q] 
        ;break
        jmp while2
    
    case4: ;znak CRET
        inc ds:[q]
        jmp while2
    
    case5:;znak NEWL
         call crc2string
         mov ax,0
         mov ds:[CRC],ax
         inc ds:[q]
         cmp ds:[VERSION],2
         je version2Case5
                   version1Case5:
                            call putchar                                   ;Wywolcac porownaj => getcrc/error       
                            jmp while2
   
                   version2Case5:  
                           call getcrc
                            call compareCRC
                            jmp while2

endOfWhile:
       
       
call close  

prints MESSAGE  
finish:                                                                         
    mov ah,4ch
    int 21h


;=======================================================================================================
;=================================================================================================
;===========   PROCEDURY       ===================================================================
;=================================================================================================
;=======================================================================================================

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

    while1:
        mov ah,ds:[81h+si]
        cmp ah,' '  ;PSP: is char a space
        je end1
            mov al,ds:[81h+ si -1]
            cmp al,' ';PSP: is previous char a space
            jne save_to_buff 
                new_arg: ; <space> <char>
                    inc bx 
                    mov es:[ARGLEN+bx-1],1h
                    add dx,2
                    pusha
                        mov bx,dx
                        mov si,offset BUFF
                        add si,di
                        mov es:[ARGOFF+bx-2],si
                    popa
                    mov es:[BUFF+di],ah ;save PSP char to buff
                    inc di   ;inc buff iterator
                    jmp end1

            save_to_buff:
                mov es:[BUFF+di],ah ;save PSP char to buff
                inc di   ;inc buff iterator
                inc es:[ARGLEN+bx-1] ;inc the length of current arg
                jmp end1

        end1:
            inc si ;inc PSP iterator
    loop while1   
    
    call fillBUFFLENGTH              
    call fillARGAMOUNT               
popa
ret
parser endp
;====================

fillBUFFLENGTH proc      
pusha
    mov bx,0
    mov ax,0
    xor dx,dx
    petla:
        mov dl,es:[ARGLEN+bx]
        cmp dl,0
        je przerwij
        add ax,dx
        inc bx
        jmp petla
    przerwij:
    mov es:[BUFFLENGTH],al
popa
ret
fillBUFFLENGTH endp

;====================
fillARGAMOUNT proc
pusha
    mov ax,dane1
    mov ds,ax
     
    mov si,0    
    countNumberOfArg:  
       mov al,ds:[ARGLEN+si]  
       cmp al,0
       je numberOfArgFound
            inc ds:[ARGAMOUNT]
               
        
    inc si    
    jmp countNumberOfArg
    
    
    numberOfArgFound:
    
popa
ret
fillARGAMOUNT endp
;==================================================================================================================================================

  
checkArg proc
pusha
    mov ax,dane1
    mov ds,ax       
    
    cmp ds:[ARGAMOUNT],2                   ;sprawdz,czy liczba argumentow wynosi 2 lub 3
    je nameInputOutput                     ;i skocz do wersji nameInputOutput albo nameVInput1Input2 albo wypisz blad
        cmp ds:[ARGAMOUNT],3
        je nameVInput1Input2  
            printe ERROR1
    
    nameInputOutput:                      
        mov ds:[VERSION],1
              
        xor cx,cx
        mov cl,byte ptr ds:[ARGLEN]   
        mov si,ds:[ARGOFF]
        mov bx,0
        copyToINPUT:    
            mov al,ds:[si+bx]
            mov ds:[INPUT+bx],al       
            inc bx 
        loop copyToINPUT 
        
        xor cx,cx
        mov cl,byte ptr ds:[ARGLEN+1]   
        mov si,ds:[ARGOFF+2]
        mov bx,0
        copyToOUTPUT:    
            mov al,ds:[si+bx]
            mov ds:[OUTPUT+bx],al       
            inc bx 
        loop copyToOUTPUT 
		
		    mov si,offset INPUT               ;Czy plik konczy sie ".txt"? Funkcja checktxt pobiera w si offset pliku ktorego ".txt" bedzie sprawdzac
			call checktxt 
			mov si, offset OUTPUT
			call checktxt
		
    jmp endCheck         
    
    nameVInput1Input2:
        mov ds:[VERSION],2		
            
            checkV:                                     ;Czy operator jest "-v"?
                cmp ds:[ARGLEN],2
                jne error2Found
                        cmp ds:[BUFF],'-'
                        jne error2Found
                                cmp ds:[BUFF+1],'v'
                                jne error2Found
                                                             
            xor cx,cx
            mov cl,byte ptr ds:[ARGLEN+1]   
            mov si,ds:[ARGOFF+2]
            mov bx,0
            copyToINPUT1:    
                mov al,ds:[si+bx]
                mov ds:[INPUT1+bx],al       
                inc bx 
            loop copyToINPUT1 
            
            xor cx,cx
            mov cl,byte ptr ds:[ARGLEN+2]   
            mov si,ds:[ARGOFF+4]
            mov bx,0
            copyToINPUT2:    
                mov al,ds:[si+bx]
                mov ds:[INPUT2+bx],al       
                inc bx 
            loop copyToINPUT2 
			
			mov si,offset INPUT1                      ;Czy plik konczy sie ".txt"? Funkcja checktxt pobiera w si offset pliku ktorego ".txt" bedzie sprawdzac
			call checktxt
			mov si, offset INPUT2
			call checktxt
			
        jmp endCheck      
        
        
    
    
    endCheck:


popa  
ret
error2Found:
				printe ERROR2
checkArg endp 

;==================================
checktxt proc ;si offset napisu
pusha       
   
    xor bx,bx
	doWhile:
		mov dl,0
		cmp ds:[si+bx],dl
		je elementBehindFound ;znaleziono offset elementu za stingiem
		inc bx
    jmp doWhile
	
	elementBehindFound:
		dec bx
		mov dl,'t'
		cmp ds:[si+bx],dl
		jne error6Found
		dec bx
		mov dl,'x'
		cmp ds:[si+bx],dl
		jne error6Found
		dec bx
		mov dl,'t'
		cmp ds:[si+bx],dl
		jne error6Found
		mov dl,'.'
		dec bx
		cmp ds:[si+bx],dl
		jne error6Found

popa
ret
error6Found:
	printe Error6
checktxt endp
;============================================================GENERUJ TABLICE=======================================================================================
generateTable proc  
pusha
    
  
       mov ax,dane1
    mov ds,ax
  
    mov ax,0  ;result
    mov bx,0  ;for(bx=0;bx<256;bx++)
    
    forAllASCII:
            mov ax,bx      ;Result:=Result xor (ord(buffer[i]) shl 8); 
            
                    mov cx,8    
                    do8Times:
                        mov dx,ax      ;if (Result and $8000)<>0 
                        and dx, 0001h 
                        cmp dx,0
                        je moveResult 
                            shr ax,1            ;Result:=(Result shl 1) xor Polynom
                            xor ax,ds:[POLYNOMIAL] 
                            jmp doneA
                        moveResult: ; else Result:=Result shl 1    
                            shr ax,1  
                        
                        doneA:
                    loop do8Times 
                  
		    push bx
			add bx,bx
            mov ds:[CRCTABLE+bx],ax 
			pop bx
            
               
                    
    inc bx
    cmp bx,255  
   
    je tableFilled      
    jmp forAllASCII
    
    tableFilled:          
popa
ret   
generateTable endp
;==============================================================================================================================================
compareCRC proc                          ;porownaj 4 pierwsze bajty CRCIMPORTED (z input2.txt) CRCSTRING (z input1.txt)
pusha 
    mov si,0  
    comparingProcess:   
        mov al,ds:[CRCIMPORTED+si]
        cmp ds:[CRCSTRING+si],al
        je charNumberSiIsCorrect
           
            prints COMMUNICATE
            mov dx,ds:[LINE]
            print dx
			jmp finish
                        
            
        charNumberSiIsCorrect:                 
        cmp si,4
        je jumpOut 
        inc si  
   jmp comparingProcess
   
   jumpOut:          
        inc ds:[LINE] 
popa 
ret
compareCRC endp   
    
   
;==================================================================================================================================================
 getcrc proc                        ;pobierz linie z CRC z pliku input2.txt i zapisz ja do CRCIMPORTED
 pusha   
    mov ah,3fh  ;write to a file
    mov dx,offset CRCIMPORTED ;gdzie zapisac
    mov bx,ds:[handle2]
    mov cx,6         
    int 21h       
    cmp ax,cx 
    jne error5Found ;jakby ostatnia linia nie miala 6 bajtow (4CRC+CRET+LF)  
        
        mov si,0
        check4Bytes:
            cmp ds:[CRCIMPORTED+si],'0'
            jb error5Found
            cmp ds:[CRCIMPORTED+si],'9'
            jbe charIsCorrect
            
                
            cmp ds:[CRCIMPORTED+si],'A'  
            jb error5Found
            cmp ds:[CRCIMPORTED+si],'F'
            ja error5Found       
            
            charIsCorrect:
                cmp si,3
                je cmpCRETNEWL
                inc si    
       jmp check4Bytes   
        
   cmpCRETNEWL:
       cmp ds:[CRCIMPORTED+4],0Dh 
       jne error5Found
       cmp ds:[CRCIMPORTED+5],0Ah
       jne error5Found
 
 popa
 ret 
 error5Found: 
     printe ERROR5 
     
 getcrc endp 
;===================================================================================================================================== 
getchar proc
pusha   
    mov ah,3fh
    mov dx,offset TEKST
    mov bx,ds:[handle1]
    mov cx,ds:[GETSIZE]
    int 21h         
    cmp ax,cx 
    je notEOF
        mov ds:[eof],1
        
    notEof:
        mov ds:[tekst_length],ax
popa  
ret
getchar endp  
     
 
 ;=========================================================================================================================================
 putchar proc
    pusha
    mov ah,40h ;write to a file
    mov cx,6 ;number of bytes to write
    mov dx,offset CRCSTRING ;ds:dx data to write
    mov bx,ds:[handle2]   
    int 21h  
    jnc noError1
        printe ERROR4              
    noError1:
    popa
    ret   
putchar endp

;=====================



;====================================================================       

open2 proc
pusha
    ;open INPUT1
    mov ah,3dh
    mov dx,offset INPUT1
    mov al,0
    int 21h  
    jnc saveInput1Handle
        printe ERROR3
    saveInput1Handle:
        mov ds:[HANDLE1],ax
                      
    ;open INPUT2 
    mov ah,3dh ;poprzednia operacja zniszczyla rejestr ax     
    mov dx,offset INPUT2  
    mov al,0
    int 21h 
    jnc saveInput2Handle
        printe ERROR3
    saveInput2Handle:   
        mov ds:[HANDLE2],ax

popa
ret
open2 endp    



;======================================================================
open1 proc  
pusha
    ;------ INPUT  
    mov ah,3dh
    mov dx,offset INPUT  
    mov al,0
    int 21h 
    jnc saveInputHandle   
        printe ERROR3
    saveInputHandle:
        mov ds:[HANDLE1],ax
    ;----- OUTPUT
    createOutput:     
        mov ah,3ch ;Open a new file/rewrite a file
        mov dx,offset OUTPUT  ;ds:dx ASCII filename
        mov cx,0 ;normal file
        int 21h   
        jnc saveOutputHandle
            printe ERROR4   
        saveOutputHandle:
            mov ds:[HANDLE2],ax  
popa
ret
open1 endp

;======================================================================
close proc 
pusha
    mov ah,3eh
    mov bx,ds:[HANDLE1]
    int 21h ;zakladam ze nie ma bledu 
    mov ah,3eh ;ax zostal zniszczony w poprzedniej operacji
    mov bx,ds:[HANDLE2]
    int 21h   
popa
ret   
close endp


;==================================================================
crc2string proc
pusha          
    mov bx,ds:[CRC] 
    xor ax,ax
    xor ax,ax
    mov si,0
    ;---------- pierwsza cyfra
    mov al,bh
    shr al,4 
    call convertAndSave 
    ;-------- druga cyfra
    mov al,bh 
    shl al,4
    shr al,4    
    call convertAndSave 
    ;-------- trzecia cyfra 
    mov al,bl 
    shr al,4
    call convertAndSave
    ;------- czwarta cyfra
    mov al,bl 
    shl al,4
    shr al,4    
    call convertAndSave    
popa
ret
crc2string endp

;--------------------------------       
convertAndSave proc  ;AH-cyfra hex ,SI indeks miejsca w CRCSTRING
    cmp al,10        ;
    jb  digit
        cmp al,0fh
        jbe letter  
            printe ERROR4 
    digit: 
        add al,'0' 
        jmp saveToCRCSTRING
    letter: 
        add al,55d 
    
    saveToCRCSTRING:
        mov ds:[CRCSTRING+si],al
        inc si 
    ret
convertAndSave endp 

;===============================       
crc16 proc    ;AX znak z bufora ktory dodajemy do CRC     
  pusha
       mov bx,dane1
       mov ds,bx
       
       mov bx,ax        ;bx = CrcTable[(ord(Buffer[i]) xor Result) and $ff];
       xor bx,ds:[CRC] 
       
       and bx,00ffh  
       add bx,bx    ;musze wziac miejces nr 2*bx z tablcy
       mov bx,ds:[CRCTABLE+bx]
       mov ax, ds:[CRC]
       shr ax,8 
       xor ax,bx
           ; xor ds:[CRC],bx    ;Result:=(Result shr 8) xor CrcTable[(ord(Buffer[i]) xor Result) and $ff];
       mov ds:[CRC],ax
    
       
    popa
    ret
crc16 endp   
    



;===========================================
;=========== KONIEC PROCEDUR            ====
;===========================================
;===========================================
code1    ends


stos1    segment stack
dw    200 dup(?)
top1    dw    ?
stos1    ends

end start1

