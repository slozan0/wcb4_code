       CHARACTER*1 AC(9),INDEL
 
      OPEN(UNIT=5,FILE="/home/slozano/VePD2_out.s1")
      OPEN(UNIT=6,FILE="/home/slozano/VePD2_ver2.s2")
******************************************************************  
        DO 1 IWRT=1,400000000
        LOW=0
        NIS=0
        NDS=0
        NA=0
        NC=0
        NG=0
        NT=0
        NI=0
        ND=0
        NID=0

        READ(5,10,END=99)(AC(I),I=1,9),NA,NC,NG,NT,NI,ND
        NID=NI+ND

      IF(NID.EQ.0) THEN
        NIS=0
        NDS=0
        GO TO 5
      ENDIF

        DO 4 ID=1,NID
        READ(5,20)IJ,INDEL
        IF(INDEL.EQ."I")NIS=NIS+IJ
        IF(INDEL.EQ."D")NDS=NDS+IJ
4     CONTINUE

5     IF(AC(9).EQ."A") LOW=1
      IF(AC(9).EQ."C") LOW=1
      IF(AC(9).EQ."G") LOW=1
      IF(AC(9).EQ."T") LOW=1
      IF(AC(9).EQ."N") LOW=1

      IF(LOW.EQ.0)THEN
        WRITE(*,13)(AC(I),I=1,9)
        WRITE(6,11)(AC(I),I=1,9),NA,NC,NG,NT,NIS,NDS
      ENDIF
!hack to prevent alphanumeric overrun of 9 as described in line 18
        IF(LOW.EQ.1)THEN
        WRITE(*,14)(AC(I),I=1,8)
        WRITE(6,12)(AC(I),I=1,8),NA,NC,NG,NT,NIS,NDS
      ENDIF

1      CONTINUE
! declaration of format to be read in 
! 
10    FORMAT(9A1,1X,4(1X,I3),2I3)
20    FORMAT(I3,1X,A1)
11    FORMAT(9A1,1X,6I10)
12    FORMAT(8A1,2X,6I10)
13    FORMAT(9A1)
14    FORMAT(8A1)
99        STOP
        END
