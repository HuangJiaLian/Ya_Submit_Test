program selfpropell
    implicit none
    integer i,j,n,dump,idum,gama,timetot,Lx,Ly,Lz,time,k,kf1,kf2,kf3,nn,len_arm,n_arm,n_x,mm
    integer nn1
    real x1(5000),y1(5000),density,xx(1000,10),yy(1000,10),x(10000),y(10000),Vx(10000),Vy(10000)
    real fx1(10000),fy1(10000),fx_c(10000),fy_c(10000),thta(5000),alpha(5000),fx,f,ff,fxx,fyy,ffy,fxx2,fyy2,ff2,ffy2
    real DT,DR,V,ran1,ran2,pi,ddt,r,eps1,rc1,col,row,fu,dx,dy,dis,w,ddd,dd,f0,m_para,m_stra,k_stra
    parameter(pi=3.1415926,r=1.0,ddt=0.002)
    parameter(timetot=5000000,Lx=36,Ly=36,Lz=20)
    real,external::rand_num
    real,external::gaussian


    open (16,file='randomnumber.txt')
    open (21,file='dd.txt')
    open (22,file='n.txt')
    open (23,file='v.txt')
    read(16,*)idum
    read(21,*)dd
    read(22,*)n_x
    read(23,*)V
    close(16)
    close(21)
    close(22)
    close(23)!

    density=0.58
    n=int((lx*ly*density)/(pi*0.5*0.5))
    write (*,*) n
    gama=100
    DT=1.0/gama
    DR=3*DT
    eps1=100.0
    Vx(:)=0
    thta(:)=0
    m_para=1.0
	m_stra=0.87
	k_stra=4.8
	f0=1.0
    !V=10
    !idum=-1169865
    rc1=1.0!1.12246
    dump=20000
    w=0.0
    kf1=1800*(1.0/gama)
    kf2=1000*(1.0/gama)
    kf3=0
    len_arm=6
    n_arm=2
    !n_x=4
!    ddd=(2*lx)/n_x
    !dd=0.95

    open (14,file='dump.1.lammpstrj')
    open (13,file='dump.100.lammpstrj')
     open(17,file='x1every.txt')
     open(18,file='y1every.txt')
     open(19,file='x2every.txt')
     open(20,file='y2every.txt')
    open(15,file='vx.txt')
    write(13,'(A15)')'ITEM:TIMESTEP'
    write(13,'(I10)')0
    write(13,'(A22)')'ITEM:NUMBER OF ATOMS'
    write(13,'(I10)') n+n_x*(len_arm*n_arm+1)
    write(13,'(A22)')'ITEM:BOX BOUNDS'
    write(13,'(A15)')'-15.0 15.0'
    write(13,'(A15)')'-15.0 15.0'
    write(13,'(A15)')'-10.0  10.0'
    write(13,'(A30)')'ITEM:ATOMS id type x y z'

!µ²°åµÄÎ»ÖÃ:star
!x(1)=0!!!!!!!!!!!iÊÇµÚ¼¸¸öarm,jÊÇarmÉÏµÚ¼¸¸ö
!y(1)=lx/2!(lx-ddd/2)/2
!nn=1
!
!
!        do i=1,n_arm
!            do j=1,len_arm
!                nn=nn+1
!            xx(i,j)=x(1)+0.25*j*cos(pi/35+(i-1)*pi*(360/n_arm)/180)!x(1)
!            yy(i,j)=y(1)+0.25*j*sin(pi/35+(i-1)*pi*(360/n_arm)/180)!x(1)
!            x(nn)=xx(i,j)
!            y(nn)=yy(i,j)
!
!     end do
!    end do
!do j=1,(n_x/2)-1
!    do i=1,nn
!    x(j*nn+i)=x(i)!+8
!    y(j*nn+i)=y(i)-j*ddd
!    end do
!end do
!do j=n_x/2,(n_x-1)
!    do i=1,nn
!        x(j*nn+i)=x(i)!+8
!        y(j*nn+i)=-y(i)+(j-n_x/2)*ddd
!    end do
!end do


!do i=2,n_x
   ! do j=1,nn
    !x(nn+(i-2)*(len_arm+1)+j)=abs(9-len_arm)*cos(2*pi*rand_num(idum))
    !y(nn*(i-2)*(len_arm+1)+j)=abs(9-len_arm)*sin(2*pi*rand_num(idum))
!end do
!
!
!
!    fx=0
!    nn1=0
!do i=1,n_x*nn
!write(13,'(2I4,3F10.4)') i,1,x(i),y(i),0.0
!end do
!close(13)
!µ²°åÎ»ÖÃ£ºend
      do i=1,n
       100 x1(i)=(lx/2.0)*(2*rand_num(idum)-1)!
           y1(i)=(ly/2.0)*(2*rand_num(idum)-1)
        do j=1,i-1
            if (sqrt((x1(i)-x1(j))**2+(y1(i)-y1(j))**2)<0.5)goto 100  !(0.9?)
        end do
        do k=1,n_x*nn
            if (sqrt((x(k)-x1(i))**2+(y(k)-y1(i))**2)<0.88)goto 100
            end do
        if(i<=n/2)then
            write(13,'(2I4,3F10.4)') n_x*nn+i,2,x1(i),y1(i),0.0
        else
            write(13,'(2I4,3F10.4)') n_x*nn+i,3,x1(i),y1(i),0.0
        end if
    end do
write(*,*) n
close(13)




 do time=1,timetot!Á£×ÓÎ»ÖÃ
    alpha(i)=0.0
    fx1(:)=0
    fy1(:)=0
!    if(time>=320000)then
!        CONTINUE
!    else
        do k=1,n!!°ôÁ£×ÓÒòLJÊÆÊÜÁ¦£¨Á£×Ó¼ä£©
            do j=1,n
                    if(j==k)then
                        continue
                    else
                        if(((k<=n/2).and.(j>n/2)).or.((k>n/2).and.(j<=n/2)))then
                            dx=x1(k)-x1(j)
                            if(dx>lx/2)dx=dx-lx
                            if(dx<-lx/2)dx=dx+lx
                            dy=y1(k)-y1(j)
                            if(dy>ly/2)dy=dy-ly
                            if(dy<-ly/2)dy=dy+ly
                            dis=sqrt(dx*dx+dy*dy)

                            if(dis<rc1) then
                                if(dis<dd)fu=kf1*(dd-dis)
                                fx1(k)=fx1(k)+fu*(dx)/dis
                                fy1(k)=fy1(k)+fu*(dy)/dis
                                if(dis>=dd)fu=kf3*(dd-dis)
                                fx1(k)=fx1(k)+fu*(dx)/dis
                                fy1(k)=fy1(k)+fu*(dy)/dis
                            end if
                        else
                            dx=x1(k)-x1(j)
                            if(dx>lx/2)dx=dx-lx
                            if(dx<-lx/2)dx=dx+lx
                            dy=y1(k)-y1(j)
                            if(dy>ly/2)dy=dy-ly
                            if(dy<-ly/2)dy=dy+ly
                            dis=sqrt(dx*dx+dy*dy)

                            if(dis<rc1) then
                                if(dis<dd)fu=kf1*(dd-dis)
                                fx1(k)=fx1(k)+fu*(dx)/dis
                                fy1(k)=fy1(k)+fu*(dy)/dis
                                if(dis>=dd)fu=kf2*(dd-dis)
                                fx1(k)=fx1(k)+fu*(dx)/dis
                                fy1(k)=fy1(k)+fu*(dy)/dis
                            end if
                        end if
                    end if
            end do
        end do
!    end if
    
    
    do i=1,n
        thta(i)=pi*(2*rand_num(idum)-1)
        fx_c(i)=(fx_c(i)+fx1(i))+f0*cos(thta(i))*(1-alpha(i))
        fy_c(i)=(fy_c(i)+fy1(i))+f0*sin(thta(i))*(1-alpha(i))
    end do
    
    do i=1,n
       200 Vx(i)=(m_para*cos(thta(i))**2+m_stra*sin(thta(i))**2)*fx_c(i)&
                &+(m_para*sin(thta(i))*cos(thta(i))-m_stra*cos(thta(i))*sin(thta(i)))*fy_c(i)
        Vy(i)=(m_para*sin(thta(i))*cos(thta(i))-m_stra*cos(thta(i))*sin(thta(i)))*fx_c(i)&
                &+(m_para*sin(thta(i))**2+m_stra*cos(thta(i))**2)*fy_c(i)
        do while(Vx(i)>1.0.or.Vx(i)<-1.0)
            Vx(i)=Vx(i)*0.1
        end do
    end do
!    open (30,file='V')
!    write(30,*) Vx(i),Vy(i)



! do k=1,n!!°ôÁ£×ÓÒòLJÊÆÊÜÁ¦£¨µ²°åÓëÁ£×Ó¼ä£©
!       do j=1,nn*n_x
!                        dx=x1(k)-x(j)
!                       ! if(dx>lx/2)dx=dx-lx
!                      ! ! if(dx<-lx/2)dx=dx+lx
!                        dy=y1(k)-y(j)
!                        dis=sqrt(dx*dx+dy*dy)
!
!                        if(dis<rc1) then
!                            if(dis<1.0)  fu=100000*(1.0-dis)
!                            fx1(k)=fx1(k)+fu*(dx)/dis
!                            fy1(k)=fy1(k)+fu*(dy)/dis
!
!                        end if
!       end do
!    end do

!!!make the move
    do i=1,n
         if(i<=n/2)then
            x1(i)=x1(i)+ddt*(V+Vx(i))
            if (x1(i)>lx/2)x1(i)=x1(i)-lx
            if (x1(i)<-lx/2)x1(i)=x1(i)+lx
             y1(i)=y1(i)!-ddt*V*sin(thta(i))
            if (y1(i)>ly/2)y1(i)=y1(i)-ly
            if (y1(i)<-ly/2)y1(i)=y1(i)+ly
        else
            x1(i)=x1(i)-ddt*(V+Vx(i))
            if (x1(i)>lx/2)x1(i)=x1(i)-lx
            if (x1(i)<-lx/2)x1(i)=x1(i)+lx
            y1(i)=y1(i)!-ddt*V*sin(thta(i))
            if (y1(i)>ly/2)y1(i)=y1(i)-ly
            if (y1(i)<-ly/2)y1(i)=y1(i)+ly
        end if

    end do
!!!end the move


         do i=1,n
         x1(i)=x1(i)+fx1(i)*ddt!*(1.0/gama)
         y1(i)=y1(i)+fy1(i)*ddt
         end do
        fxx=0
        fyy=0
        fxx2=0
        fyy2=0
         do i=1,n/2
            fxx=fxx+fx1(i)+v+Vx(i)
            fyy=fyy+fy1(i)
        end do
         ff=fxx/(n/2)
         ffy=fyy/(n/2)
         do i=(n/2)+1,n
            fxx2=fxx2+fx1(i)-v-Vx(i)
            fyy2=fyy2+fy1(i)
        end do
         ff2=fxx2/(n/2)
         ffy2=fyy2/(n/2)
        if(mod(time,1000)==0)then
            write(17,*) time,ff
            write(18,*) time,ffy
            write(19,*) time,ff2
            write(20,*) time,ffy2

        end if

        if(mod(time,dump)==0)then
        write (*,*)time
        write(14,'(A15)')'ITEM:TIMESTEP'
        write(14,'(I10)')time
        write(14,'(A22)')'ITEM:NUMBER OF ATOMS'
        write(14,'(I10)')n+n_x*(len_arm*n_arm+1)
        write(14,'(A22)')'ITEM:BOX BOUNDS'
        write(14,'(A15)')'-15.0 15.0'
        write(14,'(A15)')'-15.0 15.0'
        write(14,'(A15)')'-10.0  10.0'
        write(14,'(A30)')'ITEM:ATOMS id type x y z'

!          do i=1,nn*n_x
!            write(14,'(2I4,3F10.4)') i,1,x(i),y(i),0.0
!            end do

        do i=1,n
            if(i<=n/2)then
                write(14,'(2I4,3F10.4)') i+nn*n_x,2,x1(i),y1(i),0.0
            else
                write(14,'(2I4,3F10.4)') i+nn*n_x,3,x1(i),y1(i),0.0
            end if
        end do
        end if
         if (time>8000000)then
            nn1=nn1+1
        do i=1,n/2
            fx=fx+fx1(i)+v
        end do
        end if
end do
f=fx/((n/2)*nn1)
write (15,*) f
end




subroutine L_J(eps1,rc1,Lx,Ly,fx1,fy1,x1,y1,n)
    implicit none
    real fx1(4000),fy1(4000),x1(1000),y1(1000)
    real eps1,Ep,rc1,dis,dx,dy,Ec
    integer Lx,Ly
    double precision r2inv,r6inv,r12inv,forcelj,fpair
    integer i,j,k,l,n
  do i=1,n-1
        do k=i+1,n
                    dx=x1(i)-x1(k)
                    dy=y1(i)-y1(k)
                    dx=dx-anint(dx/Lx)*dble(Lx)
                    dy=dy-anint(dy/Ly)*dble(Ly)
                    !write(*,*)x1(i),x1(k),dx
                    dis=sqrt(dx*dx+dy*dy)
                    if(dis<rc1) then
                        r2inv=1.0/(dis*dis)
                        r6inv=r2inv*r2inv*r2inv
                        r12inv=r6inv*r6inv
                        forcelj=24.0*eps1*(2*r12inv-r6inv)
                        fpair=forcelj*r2inv
                        fx1(i)=fx1(i)+dx*fpair
                        fy1(i)=fy1(i)+dy*fpair
                        fx1(k)=fx1(k)-dx*fpair
                        fy1(k)=fy1(k)-dy*fpair
                    end if
        end do
    end do

end subroutine L_J

function gaussian(idum)   !!!!!!!different witn jiang
    implicit none
     real v1,v2,ran,rsq,fac
 	real gaussian
    integer flag,idum
 	real,external::rand_num
    flag=1
    do while(flag==1)
         ran=rand_num(idum)
        v1=2.0*ran-1.0
        ran=rand_num(idum)
        v2=2.0*ran-1.0
        rsq=v1*v1+v2*v2
        if(rsq .lt. 1.0) then!ï¼ˆitåˆ¤æ–­æ˜¯å¦å°äºŽï¼‰
 			flag=0
 			fac=sqrt(-2.0*log(rsq)/rsq)
 			gaussian=v2*fac
 		end if
    end do
    return
 end function gaussian


 function rand_num(idum)
  implicit none
  integer,parameter::ia=16807
  integer,parameter::im=2147483647
 integer,parameter::iq=127773
  integer,parameter::ir=2836
  integer,parameter::ntab=32
  integer,parameter::ndiv=1+(im-1)/ntab
  real,parameter::am=1.0/im
  real,parameter::eps=1.2E-7
  real,parameter::rnmx=1.0-eps
  integer idum,j,k
  integer,save::iv(ntab)
  integer,save::iy
  real::rand_num
  data iv /ntab*0/, iy /0/
  rand_num=0.0
  if((idum<=0).or.(iy==0)) then
    idum=max(-idum,1)
 	do j=ntab+8,1,-1
 	  k=idum/iq
 	  idum=ia*(idum-k*iq)-ir*k
 	  if(idum<0) idum=idum+im
 	  if(j<=ntab) iv(j)=idum
 	end do
 	iy=iv(1)
  end if
  k=idum/iq
  idum=ia*(idum-k*iq)-ir*k
   if(idum<0) idum=idum+im
  j=1+iy/ndiv
  iy=iv(j)
  iv(j)=idum
  rand_num=min(am*iy,rnmx)
  return
 end function rand_num

