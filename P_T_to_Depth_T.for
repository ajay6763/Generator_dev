CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     
C
      program P_T_Depth_T
c**********************************************************************
C        
C     This program convert the table from generator into depth Temperature
C     tale  
C**********************************************************************
C     
C
C**********************************************************************
      IMPLICIT double PRECISION (a-h,o-z)
      REAL k
      Parameter(pi=3.1415926,g=9.8)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      open(1,file='TABLE_LITMOD2',status='unknown')
      open(2,file='TABLE_LITMOD2_depth_T',status='unknown')
        do 20
      
c      read(3,1912,iostat=ios)aT,aP,cond
      read(1,1010,iostat=ios)aT,aP,ade,avp,avs,avpdt,avsdt,avpdp,avsdp,k
c1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6)
c1911  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6,1F8.2)
c1912  format(F9.3,1F17.4,I8.2)
c1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6)
1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6,1F12.3)
      if(IOS.lt.0.0)then
        goto 30
      else 
       aP=aP*1.0E05
       depth=(aP)/(ade*g*1000)
       write(2,1010)aT,depth,ade,avp,avs,avpdt,avsdt,avpdp,avsdp,k
c 1003 format(F9.3,1F17.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4)
c1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6)
      endif
  20    continue
30    close(1)
      close(2)
      Print *, 'Attenuation calculations are finished'
              return
      end
