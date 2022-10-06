        SUBROUTINE SMOOFT(Y,N,PTS)
!---------------------------------------------------------------
! Smooths an array Y of length N, with a window whose full width
! is of order PTS neighboring points, a user supplied value. 
! Array Y is modified.
!---------------------------------------------------------------
        Parameter(NMAX=2048)    !Double of maximum sample size
        REAL Y(NMAX)
  
          M=2
          NMIN=N+2.*PTS
        1 IF (M.LT.NMIN) THEN
            M=2*M
            GO TO 1
          END IF
          print *,' M=',M
          IF (M.GT.NMAX) Then
               print *, ' Sample too big.'
               stop
          EndIf      
          CONST=(PTS/M)**2
          Y1=Y(1)
          YN=Y(N)
          RN1=1./(N-1)
          DO J=1, N             !Remove linear trend
            Y(J)=Y(J)-RN1*(Y1*(N-J)+YN*(J-1))
          END DO
          IF (N+1.LE.M) THEN
            DO J=N+1, M
                  Y(J)=0.
            END DO
          END IF
          MO2=M/2
          CALL REALFT(Y,MO2,1)  !Fourier transform
          Y(1)=Y(1)/MO2
          FAC=1.
          DO J=1, MO2-1
            K=2*J+1
            IF (FAC.NE.0.) THEN
              FAC=AMAX1(0.,(1.-CONST*J**2)/MO2)
                  Y(K)=FAC*Y(K)
                  Y(K+1)=FAC*Y(K+1)
            ELSE
              Y(K)=0.
                  Y(K+1)=0.
            END IF
          END DO
          FAC=AMAX1(0.,(1.-0.25*PTS**2)/MO2)  !Last point
          Y(2)=FAC*Y(2)
          CALL REALFT(Y,MO2,-1)  !Inverse Fourier transform
          DO J=1, N              !Restore linear trend
            Y(J)=RN1*(Y1*(N-J)+YN*(J-1))+Y(J)
          END DO
          RETURN
        END
        
        SUBROUTINE four1(data,nn,isign) 
        INTEGER isign,nn 
        REAL data(2*nn)
        !-------------------------------------------------------------------------------------------- 
        !Replaces data(1:2*nn) by its discrete Fourier transform, if isign is
        !input as 1; or replaces 
        !data(1:2*nn) by nn times its inverse discrete Fourier transform, if
        !isign is input as -1. 
        !data is a complex array of length nn or, equivalently, a real array of
        !length 2*nn. nn 
        !MUST be an integer power of 2 (this is not checked for!). 
        !--------------------------------------------------------------------------------------------
        INTEGER i,istep,j,m,mmax,n 
        REAL tempi,tempr 
        DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp !Double precision for the trigonometric 
                                                   !recurrences. 
          n=2*nn 
          j=1 
          do i=1,n,2  !This is the bit-reversal section of the routine. 
            if(j.gt.i)then 
              tempr=data(j)  !Exchange the two complex numbers. 
              tempi=data(j+1) 
              data(j)=data(i) 
              data(j+1)=data(i+1) 
              data(i)=tempr 
              data(i+1)=tempi 
            endif 
            m=nn 
        1   if ((m.ge.2).and.(j.gt.m)) then 
              j=j-m 
              m=m/2 
              goto 1 
            endif 
            j=j+m 
          end do 
          mmax=2      !Here begins the Danielson-Lanczos section of the routine. 
        2 if (n.gt.mmax) then  !Outer loop executed log2 nn times. 
            istep=2*mmax 
            theta=6.28318530717959d0/(isign*mmax) !Initialize for the trigonometric recurrence. 
            wpr=-2.d0*sin(0.5d0*theta)**2 
            wpi=sin(theta) 
            wr=1.d0 
            wi=0.d0 
            do m=1,mmax,2  !Here are the two nested inner loops.
              do i=m,n,istep 
                j=i+mmax  !This is the Danielson-Lanczos formula: 
                tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1) 
                tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j) 
                data(j)=data(i)-tempr 
                data(j+1)=data(i+1)-tempi 
                data(i)=data(i)+tempr 
                data(i+1)=data(i+1)+tempi 
              end do 
              wtemp=wr  !Trigonometric recurrence. 
              wr=wr*wpr-wi*wpi+wr 
              wi=wi*wpr+wtemp*wpi+wi 
            end do 
            mmax=istep 
            goto 2  !Not yet done. 
          endif     !All done. 
        return 
        END 
        
        SUBROUTINE realft(data,n,isign) 
        INTEGER isign,n 
        REAL data(n) 
        ! USES four1
        !------------------------------------------------------------------------------------------- 
        !Calculates the Fourier transform of a set of n real-valued data points.
        !Replaces this data 
        !(which is stored in array data(1:n)) by the positive frequency half of
        !its complex Fourier 
        !transform. The real-valued first and last components of the complex
        !transform are returned 
        !as elements data(1) and data(2), respectively. n must be a power of 2.
        !This routine 
        !also calculates the inverse transform of a complex data array if it is
        !the transform of real 
        !data. (Result in this case must be multiplied by 2/n.) 
        !--------------------------------------------------------------------------------------------
        INTEGER i,i1,i2,i3,i4,n2p3 
        REAL c1,c2,h1i,h1r,h2i,h2r,wis,wrs 
        DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp !Double precision for trigonometric recurrences. 
          theta=3.141592653589793d0/dble(n/2)      !Initialize the recurrence. 
          c1=0.5 
          if (isign.eq.1) then 
            c2=-0.5 
            call four1(data,n/2,+1)                !The forward transform is here. 
          else
            c2=0.5                                 !Otherwise set up for aninverse transform. 
            theta=-theta 
          endif 
          wpr=-2.0d0*sin(0.5d0*theta)**2 
          wpi=sin(theta) 
          wr=1.0d0+wpr 
          wi=wpi 
          n2p3=n+3 
          do i=2,n/4  !Case i=1 done separately below. 
            i1=2*i-1 
            i2=i1+1 
            i3=n2p3-i2 
            i4=i3+1 
            wrs=sngl(wr) 
            wis=sngl(wi) 
            h1r=c1*(data(i1)+data(i3))  !The two separate transforms are separated out of data. 
                h1i=c1*(data(i2)-data(i4)) 
            h2r=-c2*(data(i2)+data(i4)) 
            h2i=c2*(data(i1)-data(i3)) 
            data(i1)=h1r+wrs*h2r-wis*h2i !Here they are recombined to form the true transform 
                                         !of the original real data. 
            data(i2)=h1i+wrs*h2i+wis*h2r 
            data(i3)=h1r-wrs*h2r+wis*h2i 
            data(i4)=-h1i+wrs*h2i+wis*h2r 
            wtemp=wr                     !The recurrence. 
            wr=wr*wpr-wi*wpi+wr 
            wi=wi*wpr+wtemp*wpi+wi 
          end do
        
          if (isign.eq.1) then 
            h1r=data(1) 
            data(1)=h1r+data(2) 
            data(2)=h1r-data(2)      !Squeeze the first and last data together to get 
                                     !them all within the original array.
          else
            h1r=data(1) 
            data(1)=c1*(h1r+data(2)) 
            data(2)=c1*(h1r-data(2)) 
            call four1(data,n/2,-1)  !This is the inverse transform for the case
            isign=0-1.0 
          end if 
          return 
        END 
