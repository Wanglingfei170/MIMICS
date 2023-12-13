      program scemain
      implicit real*8 (a-h,o-z)
c  ARRAYS FROM THE INPUT DATA
      real*8 xparam(16)
      integer nx

      nx=5
      xparam(:) = 1.0

      fa = functn(nx,xparam)  
 
c  END OF THE MAIN PROGRAM
      stop
      end

