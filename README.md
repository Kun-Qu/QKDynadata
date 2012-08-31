QKDynadata
==========

A dynamic data type for fortran, designed for numerical solving PDE

! dynamic data type

! Qu Kun, April 2012

! A simple dyanmic data container. It can manage array of INTERGER(4) 

! and REAL(8), dimension from 0 to 5. All arrays are managed in a link-list

! 

! User interface:

! dyd_new: create a aray

! dyd_getdata: fecth the pointer of a array by itd name

!

!

!  Example like:

!   TYPE(dynadata),POINTER :: list=>NULL()

!   INTEGER(4),DIMENSION(:),POINTER :: X

!   ...

!   CALL dyd_new(head=list, elementtype='integer(4)', dataname='X', ierr=ierr, &

!        range11=0,range12=9)

!   ...

!   CALL getdata(list,'X', X)

!   DO i=0,9;  X(i) = i; ENDDO

!

! The max dimension is 5. 

! To create a 5D array like: real(8) :: mat(1:5,1:5, 0:9,0:21,0:33)

!      CALL dyd_new(head=list, elementtype='real(8)', dataname='mat', ierr=ierr, &

!        range11=1,range12=5, range21=1,range22=5, &

!        range31=1,range32=9,   range41=0,range42=21, range51=0,range52=33)



