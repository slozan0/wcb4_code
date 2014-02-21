c wcb4@colostate.edu 
c last modification feb 18th 2014

      CHARACTER*1 AIN(2000),AOUT(26),ILAB(100,100),DLAB(100,100),
     +INS(100,3),DEL(100,3)
      INTEGER SKP
      
      print *, "Lets get going!"
      
      OPEN(UNIT=5,FILE="/home/slozano/VePD2.out")
      print *, "Input file open!"
      
      OPEN(UNIT=6,FILE="/home/slozano/VePD2_out.s1")
******************************************************************  
      DO 1 IA=1,130000000
C  Read in a line of Varscan -readcounts in the variable "Q" format
      READ (5,11,END=99) I,(AIN(JA),JA=1,I) 
      NI=0
      ND=0

C  Initialize a vector AOUT with 26 fields
C  AOUT will have the SNIPID in 10 fields followed by 
C a space and three fields for each of A,C,G, and T
      DO 20 IX=1,26
      AOUT(IX)=" "
20    CONTINUE

C  Initialize matrices ILAB and DLAB  that will contain up to 100 insertions and 100 deletions
C each of length 100
      DO 21 IY=1,100
      DO 22 IZ=1,100
      ILAB(IY,IZ)=" "
      DLAB(IY,IZ)=" "
22    CONTINUE
21    CONTINUE

C  Initialize a second set of matrices INS and DEL that will contain up to 
C 100 insertion and 100 deletion field each of length 3
      DO 23 IO=1,100
      DO 24 IP=1,3
        INS(IO,IP)=" "
        DEL(IO,IP)=" "
24    CONTINUE
23    CONTINUE

C Skip SNPID (9 fields) and then populate AOUT with the remaining 16 fields
      DO 30 IC=1,9
        ID=IC+8
        AOUT(IC)=AIN(ID)
30    CONTINUE

C Skip the first 17 fields of AIN and then search for the first":"
        SKP=17
40    IF(AIN(SKP).NE.":") THEN
        SKP=SKP+1
        GO TO 40
        ENDIF
C Read the field X before the ":"
        SKP=SKP-1
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C If X is an adenine (A) then skip ahead two spaces and read the number of "A's"
C Read through all of the remaining parts of the field without recording any
C information
50    IF(AIN(SKP).EQ."A")THEN

        SKP=SKP+2
        AOUT(12)=AIN(SKP)
        SKP=SKP+1        

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN 
        AOUT(13)=AIN(SKP)
        SKP=SKP+1        
        ENDIF

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN
        AOUT(14)=AIN(SKP)
        ENDIF
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C If X is a cytosine (C) then skip ahead two spaces and read the number of "C's"
C Read through all of the remaining parts of the field without recording any
C information
      IF(AIN(SKP).EQ."C")THEN

        SKP=SKP+2
        AOUT(16)=AIN(SKP)
        SKP=SKP+1        

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN 
        AOUT(17)=AIN(SKP)
        SKP=SKP+1        
        ENDIF

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN
        AOUT(18)=AIN(SKP)
        ENDIF
        ENDIF
CGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
C If X is a guanine (G) then skip ahead two spaces and read the number of "G's"
C Read through all of the remaining parts of the field without recording any
C information
      IF(AIN(SKP).EQ."G")THEN

        SKP=SKP+2
        AOUT(20)=AIN(SKP)
        SKP=SKP+1        

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN 
        AOUT(21)=AIN(SKP)
        SKP=SKP+1        
        ENDIF

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN
        AOUT(22)=AIN(SKP)
        ENDIF
        ENDIF
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C If X is a thymine (T) then skip ahead two spaces and read the number of "T's"
C Read through all of the remaining parts of the field without recording any
C information
      IF(AIN(SKP).EQ."T")THEN

        SKP=SKP+2
        AOUT(24)=AIN(SKP)
        SKP=SKP+1        

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN 
        AOUT(25)=AIN(SKP)
        SKP=SKP+1        
        ENDIF

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN
        AOUT(26)=AIN(SKP)
        ENDIF
        ENDIF
CIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
C If X is an insertion (I) then skip ahead two spaces and read the number of insertions
C Read through all of the remaining parts of the field without recording any
C information
      IF(AIN(SKP).EQ."T")THEN

        SKP=SKP+2
        AOUT(24)=AIN(SKP)
        SKP=SKP+1        

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN 
        AOUT(25)=AIN(SKP)
        SKP=SKP+1        
        ENDIF

        IF(AIN(SKP).EQ.":")GO TO 60
        IF(AIN(SKP).NE.":")THEN
        AOUT(26)=AIN(SKP)
        ENDIF
        ENDIF
      IF(AIN(SKP).EQ."I")THEN
        NI=NI+1
        ILAB(NI,1)=AIN(SKP) 

C Now read the sequence of the insertion up to 100 characters
        DO 5 II=2,100
        SKP=SKP+1
        ILAB(NI,II)=AIN(SKP) 
        IF(AIN(SKP).NE.":") GO TO 5

        IF(AIN(SKP).EQ.":")THEN
        SKP=SKP+1
        INS(NI,1)=AIN(SKP)
        SKP=SKP+1
        IF(AIN(SKP).EQ.":") GO TO 60
        IF(AIN(SKP).NE.":") INS(NI,2)=AIN(SKP)
        SKP=SKP+1
        IF(AIN(SKP).EQ.":") GO TO 60
        IF(AIN(SKP).NE.":") INS(NI,3)=AIN(SKP)
        SKP=SKP+1
        ENDIF
5        CONTINUE
        ENDIF
CDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
C If X is an deletion (D) then skip ahead two spaces and read the number of deletions
C Read through all of the remaining parts of the field without recording any
C information
      IF(AIN(SKP).EQ."D")THEN
        ND=ND+1
        DLAB(ND,1)=AIN(SKP) 

        DO 6 ID=2,100
        SKP=SKP+1
        DLAB(ND,ID)=AIN(SKP) 
        IF(AIN(SKP).NE.":") GO TO 6

        IF(AIN(SKP).EQ.":")THEN
        SKP=SKP+1
        DEL(ND,1)=AIN(SKP)
        SKP=SKP+1
        IF(AIN(SKP).EQ.":") GO TO 60
        IF(AIN(SKP).NE.":") DEL(ND,2)=AIN(SKP)
        SKP=SKP+1
        IF(AIN(SKP).EQ.":") GO TO 60
        IF(AIN(SKP).NE.":") DEL(ND,3)=AIN(SKP)
        SKP=SKP+1
        ENDIF
6      CONTINUE                
        ENDIF
C.................................................................................     

60    SKP=SKP+1
        IF(SKP.GE.I) GO TO 9
        IF(AIN(SKP).EQ."A") GO TO 50	
        IF(AIN(SKP).EQ."C") GO TO 50	
        IF(AIN(SKP).EQ."G") GO TO 50	
        IF(AIN(SKP).EQ."T") GO TO 50	
        IF(AIN(SKP).EQ."I") GO TO 50	
        IF(AIN(SKP).EQ."D") GO TO 50
        GO TO 60

9     WRITE(6,74)(AOUT(IX),IX=1,26),NI,ND

        DO 70 J=1,NI
      IF(INS(J,1).EQ." ".AND.INS(J,2).EQ." ".AND.INS(J,3).EQ." ")THEN
        INS(J,3)="0"
        ENDIF
      WRITE(6,72)(INS(J,I4),I4=1,3),(ILAB(J,I5),I5=1,50)
70    CONTINUE

        DO 75 J=1,ND
      IF(DEL(J,1).EQ." ".AND.DEL(J,2).EQ." ".AND.DEL(J,3).EQ." ")THEN
        DEL(J,3)="0"
        ENDIF
      WRITE(6,72)(DEL(J,I4),I4=1,3),(DLAB(J,I5),I5=1,100)
75    CONTINUE

       NSNP=NSNP+1
1     CONTINUE

11    FORMAT (Q,1500A1)
72    FORMAT(3A1,1X,103A1)
74    FORMAT(26A1,2I3)
      print *, "Done!"
99    STOP
        END
