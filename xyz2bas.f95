program xyztobas
implicit none
integer :: i,j,k,nbatom
integer,dimension(:), allocatable :: numat
real*8, dimension(:,:), allocatable :: coord
character*128, dimension(:), allocatable :: charatom
character*128,dimension(117):: atomchar
character*128 :: inputxyz, outputbas
logical :: stay
!==========================================================================!
!=================Valeurs des atomchar à remplacer=========================!
!==========================================================================!
atomchar(1)= "H"
atomchar(2)= "He"
atomchar(3)= "Li"
atomchar(4)= "Be"
atomchar(5)= "B"
atomchar(6)= "C"
atomchar(7)= "N" 
atomchar(8)= "O"  
atomchar(9)= "F"  
atomchar(10)= "Ne" 
atomchar(11)= "Na" 
atomchar(12)= "Mg" 
atomchar(13)= "Al" 
atomchar(14)= "Si"
atomchar(15)= "P" 
atomchar(16)= "S" 
atomchar(17)= "Cl"
atomchar(18)= "Ar"
atomchar(19)= "K" 
atomchar(20)= "Ca"
atomchar(21)= "Sc"  
atomchar(22)= "Ti"
atomchar(23)= "V" 
atomchar(24)= "Cr"
atomchar(25)= "Mn"
atomchar(26)= "Fe"
atomchar(27)= "Co"
atomchar(28)= "Ni"
atomchar(29)= "Cu" 
atomchar(30)= "Zn"
atomchar(31)= "Ga"
atomchar(32)= "Ge"
atomchar(33)= "As"
atomchar(34)= "Se"
atomchar(35)= "Br"  
atomchar(36)= "Kr"
atomchar(37)= "Rb"  
atomchar(38)= "Sr" 
atomchar(39)= "Y"
atomchar(40)= "Zr"  
atomchar(41)= "Nb"
atomchar(42)= "Mo"  
atomchar(43)= "Tc" 
atomchar(44)= "Ru"  
atomchar(45)= "Rh" 
atomchar(46)= "Pd" 
atomchar(47)= "Ag"
atomchar(48)= "Cd"
atomchar(49)= "In"
atomchar(50)= "Sn"  
atomchar(51)= "Sb"
atomchar(52)= "Te"
atomchar(53)= "I"
atomchar(54)= "Xe"
atomchar(55)= "Cs"
atomchar(56)= "Ba"
atomchar(57)= "La"
atomchar(58)= "Ce" 
atomchar(59)= "Pr" 
atomchar(60)= "Nd" 
atomchar(61)= "Pm" 
atomchar(62)= "Sm" 
atomchar(63)= "Eu"
atomchar(64)= "Gd"
atomchar(65)= "Tb"
atomchar(66)= "Dy"
atomchar(67)= "Ho"
atomchar(68)= "Er"
atomchar(69)= "Tm"
atomchar(70)= "Yb"
atomchar(71)= "Lu"
atomchar(72)= "Hf"
atomchar(73)= "Ta"
atomchar(74)= "W" 
atomchar(75)= "Re"
atomchar(76)= "Os"
atomchar(77)= "Ir"
atomchar(78)= "Pt"
atomchar(79)= "Au"
atomchar(80)= "Hg"
atomchar(81)= "Tl"
atomchar(82)= "Pb"
atomchar(83)= "Bi"
atomchar(84)= "Po"
atomchar(85)= "At"
atomchar(86)= "Rn"
atomchar(87)= "Fr"
atomchar(88)= "Ra"
atomchar(89)= "Ac"  
atomchar(90)= "Th" 
atomchar(91)= "Pa"
atomchar(92)= "U"
atomchar(93)= "Np"
atomchar(94)= "Pu"
atomchar(95)= "Am"
atomchar(96)= "Cm"
atomchar(97)= "Bk"
atomchar(98)= "Cf"
atomchar(99)= "Es"
atomchar(100)= "Fm"
atomchar(101)= "Md"
atomchar(102)= "No"
atomchar(103)= "Lr"
atomchar(104)= "Rf"
atomchar(105)= "Db"
atomchar(106)= "Sg"
atomchar(107)= "Bh"
atomchar(108)= "Hs"
atomchar(109)= "Mt"
atomchar(110)= "Ds"
atomchar(111)= "Rg"
atomchar(112)= "Cn"
atomchar(113)= "Uut" 
atomchar(114)= "Fl"
atomchar(115)= "Uup"
atomchar(116)= "Lv"
atomchar(117)= "Uuh"

!===========================================================================!
!=============================lecture fichier xyz===========================!
!===========================================================================!

call getarg(1,inputxyz)
inputxyz=trim(inputxyz)
open(10,file=inputxyz,status='old',action='read')
read(10,*) nbatom
allocate(charatom(nbatom),coord(nbatom,3),numat(nbatom))
read(10,*)
do i = 1, nbatom
   read(10,*) charatom(i),(coord(i,j),j=1,3)
enddo
 close(10)
!==========================================================================!
!===============attribution du numéro atomique=============================!
!==========================================================================!

do i=1,nbatom
   do j=1,117
         if (charatom(i) == atomchar(j)) then
               numat(i)=j
         endif
   enddo
enddo

!==========================================================================!
!===================ecriture du fichier output=============================!
!==========================================================================!
call getarg(2,outputbas)
outputbas=trim(outputbas)

open(55,file=outputbas,action='write')
write(55,'(9x,i3)') nbatom
do i = 1,nbatom
   write(55,'(1x,i3,4x,f10.6,4x,f10.6,4x,f10.6)') numat(i),coord(i,1)-coord(1,1),coord(i,2)-coord(1,2),coord(i,3)-coord(1,3)
enddo
 close(55)

!*************************************************************************!
!*****************libération de la mémoire********************************!
!*************************************************************************!
deallocate(numat,charatom,coord)

endprogram





































