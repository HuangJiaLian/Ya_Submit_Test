program rannum
  implicit none
  integer n
  integer::status=0
  open(11,file='randomnumber.txt')
  open(12,file='randomnumber1.txt')
  read(11,*) n
  do while(1)
    read(11,fmt=*,iostat=status) n
	if(status/=0) exit
	write(12,*) n
  end do
  close(11)
  close(12)
end