T=64
x0=.01
A0=100
k(x)=pi*(x-x0)*T
km1(x)=2*pi*(x-x0)*T - 1.0
kp1(x)=2*pi*(x-x0)*T + 1.0
sinc(x) = (x==0 ? 1: sin(k(x))/k(x))
ha2(x)= 0.25*(kp1(x)==0 ? 1 : sin(kp1(x))/kp1(x)) + 0.5*(k(x)==0 ? 1: sin(k(x))/k(x)) + 0.25*(km1(x)==0 ? 1 : sin(km1(x))/km1(x))
den(x)=2.0*pi*T*(x)*(T*T*x*x - 1.0)
ha(x)=-sin(pi*T*(x-x0))/den(x-x0)
hadb(x)=10*log(abs(ha(x))) + A0
