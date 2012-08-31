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

MODULE qkdynadata
IMPLICIT NONE
INTEGER,PARAMETER,PRIVATE :: dyd_maxdim=5,dyd_namelen=32
INTEGER,PARAMETER,PRIVATE :: int4=1,real8=2
TYPE dynadata
  INTEGER :: mdim=-1
  INTEGER :: mrange(2,dyd_maxdim)=-1
  INTEGER :: metype=-1  ! data type of the element, integer(4), real(8),real(4)
  CHARACTER(LEN=dyd_namelen) mname
 
  INTEGER(4),POINTER :: mint4d0p=>NULL()
  INTEGER(4),POINTER,DIMENSION(:) :: mint4d1p=>NULL()
  INTEGER(4),POINTER,DIMENSION(:,:) :: mint4d2p=>NULL()
  INTEGER(4),POINTER,DIMENSION(:,:,:) :: mint4d3p=>NULL()
  INTEGER(4),POINTER,DIMENSION(:,:,:,:) :: mint4d4p=>NULL()
  INTEGER(4),POINTER,DIMENSION(:,:,:,:,:) :: mint4d5p=>NULL()
 
  REAL(8),POINTER :: mreal8d0p=>NULL()
  REAL(8),POINTER,DIMENSION(:) :: mreal8d1p=>NULL()
  REAL(8),POINTER,DIMENSION(:,:) :: mreal8d2p=>NULL()
  REAL(8),POINTER,DIMENSION(:,:,:) :: mreal8d3p=>NULL()
  REAL(8),POINTER,DIMENSION(:,:,:,:) :: mreal8d4p=>NULL()
  REAL(8),POINTER,DIMENSION(:,:,:,:,:) :: mreal8d5p=>NULL()
 
  !pointer for inner dynamic data member
  TYPE(dynadata),POINTER :: mdynadata
 
  ! pointer for linklist
  TYPE(dynadata),POINTER :: mprev,mnext,mhead
 
END TYPE
 
 
INTERFACE getdata
  MODULE PROCEDURE getdata_int4p0
  MODULE PROCEDURE getdata_int4p1
  MODULE PROCEDURE getdata_int4p2
  MODULE PROCEDURE getdata_int4p3
  MODULE PROCEDURE getdata_int4p4
  MODULE PROCEDURE getdata_int4p5
  MODULE PROCEDURE getdata_real8p0
  MODULE PROCEDURE getdata_real8p1
  MODULE PROCEDURE getdata_real8p2
  MODULE PROCEDURE getdata_real8p3
  MODULE PROCEDURE getdata_real8p4
  MODULE PROCEDURE getdata_real8p5
END INTERFACE
 
CONTAINS
 
SUBROUTINE getdata_int4p0(head,aname, dp)
  INTEGER(4),POINTER :: dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  IF(ASSOCIATED(iter)) dp => iter%mint4d0p
END SUBROUTINE
 
SUBROUTINE getdata_int4p1(head,aname,dp)
  INTEGER(4),DIMENSION(:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
 ! WRITE(*,*) iter%mname
  dp => iter%mint4d1p
END SUBROUTINE
 
SUBROUTINE getdata_int4p2(head,aname,dp)
  INTEGER(4),DIMENSION(:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mint4d2p
END SUBROUTINE
 
 
SUBROUTINE getdata_int4p3(head,aname,dp)
  INTEGER(4),DIMENSION(:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mint4d3p
END SUBROUTINE
 
 
SUBROUTINE getdata_int4p4(head,aname,dp)
  INTEGER(4),DIMENSION(:,:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mint4d4p
END SUBROUTINE
 
 
SUBROUTINE getdata_int4p5(head,aname,dp)
  INTEGER(4),DIMENSION(:,:,:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mint4d5p
END SUBROUTINE
 
 
 
 
 
 
 
SUBROUTINE getdata_real8p0(head,aname, dp)
  REAL(8),POINTER :: dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d0p
END SUBROUTINE
 
SUBROUTINE getdata_real8p1(head,aname,dp)
  REAL(8),DIMENSION(:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d1p
END SUBROUTINE
 
SUBROUTINE getdata_real8p2(head,aname,dp)
  REAL(8),DIMENSION(:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d2p
END SUBROUTINE
 
 
SUBROUTINE getdata_real8p3(head,aname,dp)
  REAL(8),DIMENSION(:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d3p
END SUBROUTINE
 
 
SUBROUTINE getdata_real8p4(head,aname,dp)
  REAL(8),DIMENSION(:,:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d4p
END SUBROUTINE
 
 
SUBROUTINE getdata_real8p5(head,aname,dp)
  REAL(8),DIMENSION(:,:,:,:,:),POINTER ::dp
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
  NULLIFY(dp)
  iter => dyd_findnode(head,aname)
  dp => iter%mreal8d5p
END SUBROUTINE
 
 
 
 
SUBROUTINE dyd_empty(this)
  TYPE(dynadata),INTENT(INOUT) :: this
  ! local
  LOGICAL fk
  INTEGER ierr
  this%mdim=-1; this%mrange=-1; this%metype=-1; this%mname=''
 
  NULLIFY(this%mdynadata)
  NULLIFY(this%mprev)
  NULLIFY(this%mnext)
  NULLIFY(this%mhead)
  IF(ASSOCIATED(this%mint4d0p)) THEN
    DEALLOCATE(this%mint4d0p,STAT=ierr);    NULLIFY(this%mint4d0p)
  ENDIF 
   
 
  IF(ASSOCIATED(this%mint4d1p)) THEN
    DEALLOCATE(this%mint4d1p,STAT=ierr)
    NULLIFY(this%mint4d1p)
  ENDIF
 
  IF(ASSOCIATED(this%mint4d2p)) THEN
    DEALLOCATE(this%mint4d2p,STAT=ierr)
    NULLIFY(this%mint4d2p)
  ENDIF
   
  IF(ASSOCIATED(this%mint4d3p)) THEN
    DEALLOCATE(this%mint4d3p,STAT=ierr)
    NULLIFY(this%mint4d3p)
  ENDIF
  IF(ASSOCIATED(this%mint4d4p)) THEN
    DEALLOCATE(this%mint4d4p,STAT=ierr)
    NULLIFY(this%mint4d4p)
  ENDIF
   
  IF(ASSOCIATED(this%mint4d5p)) THEN
    DEALLOCATE(this%mint4d5p,STAT=ierr)
    NULLIFY(this%mint4d5p)
  ENDIF
 
  IF(ASSOCIATED(this%mreal8d0p)) THEN
    DEALLOCATE(this%mreal8d0p,STAT=ierr); NULLIFY(this%mreal8d0p)
  ENDIF
 
  IF(ASSOCIATED(this%mreal8d1p)) THEN
    DEALLOCATE(this%mreal8d1p,STAT=ierr); NULLIFY(this%mreal8d1p)
  ENDIF
   
  IF(ASSOCIATED(this%mreal8d2p)) THEN
    DEALLOCATE(this%mreal8d2p,STAT=ierr); NULLIFY(this%mreal8d2p)
  ENDIF
   
  IF(ASSOCIATED(this%mreal8d3p)) THEN
    DEALLOCATE(this%mreal8d3p,STAT=ierr); NULLIFY(this%mreal8d3p)
  ENDIF
   
  IF(ASSOCIATED(this%mreal8d4p)) THEN
    DEALLOCATE(this%mreal8d4p,STAT=ierr); NULLIFY(this%mreal8d4p)
  ENDIF
  IF(ASSOCIATED(this%mreal8d5p)) THEN
    DEALLOCATE(this%mreal8d5p,STAT=ierr); NULLIFY(this%mreal8d5p)
  ENDIF
 
END SUBROUTINE
 
 
 
 
 
SUBROUTINE dyd_new(head,elementtype, dataname, ierr,  &
          range11, range12,range21, range22,range31, range32, &
          range41, range42,range51, range52)
  TYPE(dynadata),POINTER,INTENT(INOUT) :: head
  CHARACTER(LEN=*),INTENT(IN) :: elementtype, dataname
  INTEGER,INTENT(IN),OPTIONAL :: range11, range12,range21, range22,  &
          range31, range32, range41, range42,range51, range52
  INTEGER,INTENT(OUT) :: ierr
  ! local
  INTEGER :: atype,adim
  INTEGER :: arange(2,10)
  TYPE(dynadata),POINTER :: pdyd
  CHARACTER(LEN=32) :: tstr
  
  ierr=0
  
  IF(.NOT.dyd_isvalidname(head,dataname)) THEN
   ierr=1
   RETURN
  ENDIF
  
  ! get type
  tstr = TRIM(elementtype)
  CALL  upper_case(tstr)
  IF(tstr=='INTEGER(4)') THEN;    atype=int4
  ELSEIF(tstr=='REAL(8)') THEN;   atype=real8
  ELSE
    ierr=2
    RETURN
  ENDIF
  
  ! get
  adim=0
  IF( PRESENT(range11) .AND. PRESENT(range12) ) THEN
    IF(range11>=range12) THEN
      WRITE(*,*) 'Invalid range'; ierr=3
      RETURN
    ENDIF
    adim=1; arange(1,1)=range11; arange(2,1)=range12
    ! DIM 2
    IF( PRESENT(range21) .AND. PRESENT(range22) ) THEN
      IF(range21>=range22) THEN
        WRITE(*,*) 'Invalid range'; ierr=3
      RETURN
      ENDIF
      adim=2; arange(1,2)=range21; arange(2,2)=range22
      ! DIM 3
      IF( PRESENT(range31) .AND. PRESENT(range32) ) THEN
        IF(range31>=range32) THEN
          WRITE(*,*) 'Invalid range'; ierr=3
          RETURN
        ENDIF
        adim=3; arange(1,3)=range31; arange(2,3)=range32
        ! DIM 4
        IF( PRESENT(range41) .AND. PRESENT(range42) ) THEN
          IF(range41>=range42) THEN
            WRITE(*,*) 'Invalid range'; ierr=3
            RETURN
          ENDIF
          adim=4; arange(1,4)=range41; arange(2,4)=range42
          ! DIM 5
          IF( PRESENT(range51) .AND. PRESENT(range52) ) THEN
            IF(range51>=range52) THEN
              WRITE(*,*) 'Invalid range'; ierr=3
              RETURN
            ENDIF
            adim=5; arange(1,5)=range51; arange(2,5)=range52
            ENDIF
          ENDIF
      ENDIF
    ENDIF
  ENDIF
 
  !WRITE(*,*) 'dyd_new ',atype,adim,arange(:,1:adim),dataname 
  CALL dyd_new_0(head,atype,adim,arange,dataname)
END SUBROUTINE
 
 
 
 
SUBROUTINE dyd_new_1(head,elementtype, arrayrange, dataname, ierr)
  TYPE(dynadata),POINTER,INTENT(INOUT) :: head
  CHARACTER(LEN=*),INTENT(IN) :: elementtype, arrayrange,dataname
  INTEGER,INTENT(OUT) :: ierr
  ! local
  INTEGER :: atype,offset,suboffset,i,adim
  INTEGER :: arange(2,10), loc(20),com(10)
  TYPE(dynadata),POINTER :: pdyd
  CHARACTER(LEN=32) :: tstr

  !WRITE(*,*) 'new1 ',dataname
 
  ierr=0
  
  IF(.NOT.dyd_isvalidname(head,dataname)) THEN
   ierr=1
   RETURN
  ENDIF
  
  ! get type
  tstr = TRIM(elementtype)
  CALL  upper_case(tstr)
  IF(tstr=='INTEGER(4)') THEN;    atype=int4
  ELSEIF(tstr=='REAL(8)') THEN;   atype=real8
  ELSE
    ierr=2
    RETURN
  ENDIF
  
  ! get
  adim=0; offset=1
  DO
    suboffset = INDEX(arrayrange(offset:),':')
    IF(suboffset==0) EXIT
    adim = adim+1
    IF(adim>5) STOP
    offset = offset+suboffset
  ENDDO
  !WRITE(*,*) 'adim=',adim,'   ',arrayrange
  
  IF(adim>0) THEN
      loc(1) = INDEX(arrayrange,'(')
      loc(adim+1) = INDEX(arrayrange,')')
      com(1) = INDEX(arrayrange,':')
      IF(adim>1) THEN
      DO i=2,adim
        loc(i) = loc(i-1)+INDEX(arrayrange(loc(i-1)+1:),',')
        !write(*,*) loc(i),'   ',arrayrange(loc(i-1)+1:)
        com(i) = loc(i)+INDEX(arrayrange(loc(i)+1:),':')
        !write(*,*) com(i),'   ',arrayrange(loc(i)+1:)
      ENDDO
      ENDIF
      write(*,*) loc(1:adim+1)
      write(*,*) com(1:adim)
      DO i=1,adim
        READ( arrayrange(loc(i)+1 : com(i)-1  ),  *)  arange(1,i)
        READ( arrayrange(com(i)+1 : loc(i+1)-1),  *)  arange(2,i)
      ENDDO
  ENDIF
  
  CALL dyd_new_0(head,atype,adim,arange,dataname)
  !NULLIFY(pdyd)
  !ALLOCATE(pdyd)
  !CALL dyd_construct(pdyd, atype,adim,arange,dataname)
  !CALL dyd_insert(pdyd,head)
END SUBROUTINE
 
 
SUBROUTINE dyd_new_0(head,atype,adim,arange,aname)
  TYPE(dynadata),POINTER,INTENT(INOUT) :: head
  INTEGER,INTENT(IN) :: atype,adim
  INTEGER,INTENT(IN),OPTIONAL :: arange(2,adim)
  CHARACTER(LEN=*),INTENT(IN) :: aname
  ! local
  TYPE(dynadata),POINTER :: pdyd

 
  NULLIFY(pdyd)
  IF(dyd_isvalidname(head,aname)) THEN
    ALLOCATE(pdyd)
    CALL dyd_construct(pdyd, atype,adim,arange,aname)
    CALL dyd_insert(pdyd,head)
  ENDIF
END SUBROUTINE
 
 
 
 
SUBROUTINE dyd_construct(this,atype,adim,arange,aname)
  TYPE(dynadata),INTENT(INOUT) :: this
  INTEGER,INTENT(IN) :: atype,adim
  INTEGER,INTENT(IN),OPTIONAL :: arange(2,adim)
  CHARACTER(LEN=*),INTENT(IN) :: aname
  !
  INTEGER i
 
  CALL dyd_empty(this)
  IF(atype<=0 .OR. atype>2) THEN
    WRITE(*,*) 'Invalid type: ',atype
    STOP
  ENDIF
 
  IF(adim<0) THEN
    WRITE(*,*) 'Invalid dim: ',adim
    STOP
  ENDIF
 
  IF(adim>0) THEN;  DO i=1,adim
    IF(arange(1,i)>arange(2,i)) THEN
      WRITE(*,*) 'Invalid range: ',arange(1:2,i), ' in DIM ',i
      STOP
    ENDIF
  ENDDO;  ENDIF
 
  this%mname = aname
  this%metype=atype
  this%mdim=adim
  IF(adim>0) this%mrange(1:2,1:adim) = arange(1:2,1:adim)
 
  SELECT CASE(this%metype)
    CASE (int4)
       IF(this%mdim==0) THEN
         ALLOCATE(this%mint4d0p)
       ENDIF
       IF(this%mdim==1) THEN
         ALLOCATE(this%mint4d1p(this%mrange(1,1):this%mrange(2,1) ) )
       ENDIF
       IF(this%mdim==2) THEN
         ALLOCATE(this%mint4d2p(this%mrange(1,1):this%mrange(2,1), &
                                this%mrange(1,2):this%mrange(2,2) ) )
       ENDIF
       IF(this%mdim==3) THEN
         ALLOCATE(this%mint4d3p(this%mrange(1,1):this%mrange(2,1), &
                                this%mrange(1,2):this%mrange(2,2), &
                                this%mrange(1,3):this%mrange(2,3) ) )
       ENDIF
       IF(this%mdim==4) THEN
         ALLOCATE(this%mint4d4p(this%mrange(1,1):this%mrange(2,1), &
                                this%mrange(1,2):this%mrange(2,2), &
                                this%mrange(1,3):this%mrange(2,3), &
                                this%mrange(1,4):this%mrange(2,4) ) )
       ENDIF
       IF(this%mdim==5) THEN
         ALLOCATE(this%mint4d5p(this%mrange(1,1):this%mrange(2,1), &
                                this%mrange(1,2):this%mrange(2,2), &
                                this%mrange(1,3):this%mrange(2,3), &
                                this%mrange(1,4):this%mrange(2,4), &
                                this%mrange(1,5):this%mrange(2,5) ) )
       ENDIF
    CASE (real8)
       IF(this%mdim==0) THEN
         ALLOCATE(this%mreal8d0p)
       ENDIF
       IF(this%mdim==1) THEN
         ALLOCATE(this%mreal8d1p(this%mrange(1,1):this%mrange(2,1) ) )
       ENDIF
       IF(this%mdim==2) THEN
         ALLOCATE(this%mreal8d2p(this%mrange(1,1):this%mrange(2,1), &
                                 this%mrange(1,2):this%mrange(2,2) ) )
       ENDIF
       IF(this%mdim==3) THEN
         ALLOCATE(this%mreal8d3p(this%mrange(1,1):this%mrange(2,1), &
                                 this%mrange(1,2):this%mrange(2,2), &
                                 this%mrange(1,3):this%mrange(2,3) ) )
       ENDIF
       IF(this%mdim==4) THEN
         ALLOCATE(this%mreal8d4p(this%mrange(1,1):this%mrange(2,1), &
                                 this%mrange(1,2):this%mrange(2,2), &
                                 this%mrange(1,3):this%mrange(2,3), &
                                 this%mrange(1,4):this%mrange(2,4) ) )
       ENDIF
       IF(this%mdim==5) THEN
         ALLOCATE(this%mreal8d5p(this%mrange(1,1):this%mrange(2,1), &
                                 this%mrange(1,2):this%mrange(2,2), &
                                 this%mrange(1,3):this%mrange(2,3), &
                                 this%mrange(1,4):this%mrange(2,4), &
                                 this%mrange(1,5):this%mrange(2,5) ) )
       ENDIF
  END SELECT
END SUBROUTINE
 
 
 
SUBROUTINE dyd_insert(this,head)
  TYPE(dynadata),POINTER,INTENT(INOUT) :: this
  TYPE(dynadata),POINTER,INTENT(INOUT) :: head
  !
  TYPE(dynadata),POINTER :: tail
  IF( ASSOCIATED(head) ) THEN
    CALL dyd_gettail(head,tail)
    !WRITE(*,*) 'Tail name is ',TRIM(tail%mname)
    this%mprev => tail
    this%mhead => head
    tail%mnext => this
    NULLIFY(this%mnext)
  ELSE
    head => this
    this%mprev => NULL()
    this%mhead => this
    this%mnext => NULL()
  ENDIF
END SUBROUTINE
 
SUBROUTINE dyd_gettail(this,tail)
  TYPE(dynadata),POINTER,INTENT(IN) :: this
  TYPE(dynadata),POINTER,INTENT(OUT) :: tail
 
  IF(ASSOCIATED(this)) THEN
    tail => this
    DO WHILE( ASSOCIATED(tail%mnext) )
      tail => tail%mnext
    END DO
  ENDIF
 
END SUBROUTINE
 
 
FUNCTION  dyd_isvalidname(head,aname)
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  LOGICAL dyd_isvalidname
  !
  TYPE(dynadata),POINTER :: iter
 
  dyd_isvalidname = .TRUE.
  IF( ASSOCIATED(head) ) THEN
    iter=>head
    DO WHILE( ASSOCIATED(iter%mnext) )
    !WRITE(*,*) TRIM(aname), '  ',TRIM(iter%mname)
    !READ(*,*)
    IF(TRIM(aname)==TRIM(iter%mname)) THEN
      dyd_isvalidname = .FALSE.
      EXIT
    ENDIF
    iter => iter%mnext
    END DO
  ENDIF
END FUNCTIOn
 
FUNCTION dyd_findnode(head,aname)
  TYPE(dynadata),POINTER,INTENT(IN) :: head
  CHARACTER(LEN=*),INTENT(IN) :: aname
  TYPE(dynadata),POINTER :: dyd_findnode
  !
  TYPE(dynadata),POINTER :: iter=>NULL()
 
  NULLIFY(dyd_findnode)
  IF(.NOT.ASSOCIATED(head)) RETURN
  iter=>head
  loop: DO
    IF(TRIM(aname)==TRIM(iter%mname)) THEN
      dyd_findnode => iter   ! 找到
      EXIT loop
    ENDIF
    IF(.NOT.ASSOCIATED(iter%mnext)) THEN
      EXIT loop              ! 队列结束，没找到
    ELSE
      iter=>iter%mnext
    ENDIF
  END DO loop
   
END FUNCTION
 
 
 
 
! other
elemental subroutine upper_case(word)
! convert a word to lower case
character (len=*) , intent(in out) :: word
integer :: i,ic,nlen
nlen = len(word)
do i=1,nlen
   ic = ichar(word(i:i))
   if (ic >= 97 .and. ic < 122) word(i:i) = char(ic-32)
end do
end subroutine upper_case


 
END MODULE
 
 

