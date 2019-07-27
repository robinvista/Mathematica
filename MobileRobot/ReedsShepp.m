(* ::Package:: *)

(*\:8ba1\:7b97ReedsShepp\:66f2\:7ebf\:5e76\:751f\:6210\:79bb\:6563\:8def\:5f84\:70b9*)


BeginPackage["ReedsShepp`"]; 
(*Rmin=Global`Rmin;*)(*\:6700\:5c0f\:8f6c\:5411\:534a\:5f84Rmin\:5b9a\:4e49\:6210\:5168\:5c40\:53d8\:91cf\:ff0cnum,RSl1,RSl2,RSl3\:4e5f\:662f\:5168\:5c40\:53d8\:91cf*)
EPS=10^(-10);(*\:8bef\:5dee\:9608\:503c*)
mod2pi::usage ="\:5c06\:89d2\:5ea6\:503c\:8f6c\:6362\:52300~2pi\:7684\:8303\:56f4\:5185"
atan::usage ="\:53cd\:6b63\:5207\:ff0c\:8003\:8651\:4e86\:70b9\:5728\:539f\:70b9\:7684\:7279\:4f8b"
c1c1c::usage ="c|c|c\:7c7b\:578b"
c1cc::usage ="c|cc\:7c7b\:578b"
cc1c::usage ="cc|c\:7c7b\:578b"
csca::usage =" "
cscb::usage ="  "
ccu1cuc::usage =" "
c1cucu1c::usage =" "
c1c2sca::usage =" "
c1c2scb::usage =" "
c1c2sc21c::usage =" "
csc21ca::usage =" "
csc21cb::usage =" "
ReedShepp::usage ="\:8f93\:5165\:4e3a\:8d77\:6b62\:4f4d\:59ff\:5750\:6807\:ff0c\:8ba1\:7b97Reeds-Shepp\:66f2\:7ebf"
fctcurve::usage ="\:751f\:6210\:5404\:5206\:6bb5\:8def\:5f84\:4e0a\:4e00\:7cfb\:5217\:79bb\:6563\:70b9"
constRS::usage ="\:5c06fctcurve\:51fd\:6570\:751f\:6210\:7684\:51e0\:6bb5\:8def\:5f84\:70b9\:5408\:6210\:603b\:8def\:5f84"
(*Begin["`Private`"];*)


mod2pi[angle_]:=Module[{angletmp=Re[angle]},
	While[angletmp<0,angletmp=angletmp+2Pi];
	While[angletmp>=2Pi,angletmp=angletmp-2Pi];
	angletmp
];
atan[y_,x_]:=If[{x,y}=={0,0},0,mod2pi@ArcTan[x,y]];


c1c1c[x_,y_,\[Phi]_,rs_,rc_]:=
   Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},(*t,u,v\:662f\:4e09\:6bb5\:66f2\:7ebf\:7684\:957f\:5ea6\:503c\:6216\:5f27\:5ea6\:503c*)
      a=x-rs;
      b=y+rc;
      If[(Abs[a]<EPS)&&(Abs[b]<EPS),Return[{Infinity,0,0,0}]];
      u1=Sqrt[a^2+b^2];
      If[Re[u1]>4Rmin,Return[{Infinity,0,0,0}]];
      \[Theta]=atan[b,a];
      \[Alpha]=ArcCos[u1/(4Rmin)];
      t=mod2pi[Pi/2+\[Alpha]+\[Theta]];
      u=mod2pi[Pi-2*\[Alpha]];
      v=mod2pi[\[Phi]-t-u];
      lengthrs=Rmin*(t+u+v);
      Return[{lengthrs,t,u,v}];
];

c1cc[x_,y_,\[Phi]_,rs_,rc_]:=
   Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,result,t,u,v},
      a=x-rs;
      b=y+rc;
      If[(Abs[a]<EPS)&&(Abs[b]<EPS),Return[{Infinity,0,0,0}]];
      u1=Sqrt[a^2+b^2];
      If[Re[u1]>4Rmin,Return[{Infinity,0,0,0}]];
      \[Theta]=atan[b,a];
      \[Alpha]=ArcCos[u1/(4Rmin)];
      t=mod2pi[Pi/2+\[Alpha]+\[Theta]];
      u=mod2pi[Pi-2\[Alpha]];
      v=mod2pi[t+u-\[Phi]];
      lengthrs=Rmin(t+u+v);
      Return[{lengthrs,t,u,v}];
];
cc1c[x_,y_,\[Phi]_,rs_,rc_]:=
Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,va,t,u,v},
      a=x-rs;
      b=y+rc;
      If[(Abs[a]<EPS)&&(Abs[b]<EPS), Return[{Infinity,0,0,0}]];
      u1=Sqrt[a^2+b^2];
      If[Re[u1]>4Rmin, Return[{Infinity,0,0,0}]];
      \[Theta]=atan[b,a];
      u=ArcCos[(8Rmin^2-u1^2)/(8Rmin^2)];
      va=Sin[u];
      If[Abs[va]<0.001, va=0.0];
      If[(Abs[va]<0.001)&&(Abs[u1]<0.001),Return[{Infinity,0,0,0}]];
      \[Alpha]=ArcSin[2Rmin va/u1];
      t=mod2pi[Pi/2-\[Alpha]+\[Theta]];
      v=mod2pi[t-u-\[Phi]];
      lengthrs=Rmin*(t+u+v);
      Return[{lengthrs,t,u,v}];
];
csca[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,lengthrs,t,u,v},
	a=x-rs;
	b=y+rc;
	t=mod2pi[atan[b,a]];
	u=Sqrt[a^2+b^2];
	v=mod2pi[\[Phi]-t];
	lengthrs=Rmin*(t+v)+ u;
	Return[{lengthrs,t,u,v}];
];
 
cscb[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
   a=x+rs;
   b=y-rc;
   u1=Sqrt[a^2+b^2];
   If[Re[u1]<2Rmin, Return[{Infinity,0,0,0}]];
   \[Theta]=atan[b,a];
   u=Sqrt[u1^2-4Rmin^2];
   \[Alpha]=atan[2Rmin,u];
   t=mod2pi[\[Theta]+\[Alpha]];
   v=mod2pi[t-\[Phi]];
   lengthrs=Rmin(t+v)+ u;
   Return[{lengthrs,t,u,v}];
];

ccu1cuc[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
   a=x+rs;
   b=y-rc;
   If[(Abs[a]<EPS)&&(Abs[b]<EPS), Return[{Infinity,0,0,0}]];
   u1=Sqrt[a^2+b^2];
   If[Re[u1]>4Rmin,Return[{Infinity,0,0,0}]];
   \[Theta]=atan[b,a];
   If[Re[u1]>2Rmin, 
      {\[Alpha]=ArcCos[(u1/2-Rmin)/(2Rmin)];
      t=mod2pi[Pi/2+\[Theta]-\[Alpha]];
      u=mod2pi[Pi-\[Alpha]];
      v=mod2pi[\[Phi]-t+2u];},
      {\[Alpha]=ArcCos[(u1/2+Rmin)/(2Rmin)];
      t=mod2pi[Pi/2+\[Theta]+\[Alpha]];
      u=mod2pi[\[Alpha]];
      v=mod2pi[\[Phi]-t+2u];}];
   lengthrs=Rmin*(2u+t+v);
   Return[{lengthrs,t,u,v}];
];

c1cucu1c[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,va1,va2,t,u,v},
	a=x+rs;
	b=y-rc;
	If[(Abs[a]<EPS)&&(Abs[b]<EPS),Return[{Infinity,0,0,0}]];
	u1=Sqrt[a^2+b^2];
	If[Re[u1]>6Rmin,Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	va1=(5(Rmin^2)-u1^2/4)/(4Rmin^2);
	If[(va1<0.0)||(va1>1.0), Return[{Infinity,0,0,0}]];
	u=ArcCos[va1];
	va2=Sin[u];
	\[Alpha]=ArcSin[2Rmin*va2/u1];
	t=mod2pi[Pi/2+\[Theta]+\[Alpha]];
	v=mod2pi[t-\[Phi]];
	lengthrs=Rmin(2u+t+v);
	Return[{lengthrs,t,u,v}];
];

c1c2sca[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
	a=x-rs;
	b=y+rc;
	u1=Sqrt[a^2+b^2];
	If[Re[u1]<2Rmin,Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	u=Sqrt[u1^2-(4Rmin^2)]-(2Rmin);
	If[Re[u]<0, Return[{Infinity,0,0,0}]];
	\[Alpha]=atan[(2Rmin),(u+2Rmin)];
	t=mod2pi[Pi/2+\[Theta]+\[Alpha]];
	v=mod2pi[t+Pi/2-\[Phi]];
	lengthrs=Rmin*(t+Pi/2+v)+u;
	Return[{lengthrs,t,u,v}];
]; 

c1c2scb[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],lengthrs,t,u,v},
	a=x+rs;
	b=y-rc;
	u1=Sqrt[a^2+b^2];
	If[Re[u1]<2Rmin, Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	t=mod2pi[Pi/2+\[Theta]];
	u=u1-2Rmin;
	v=mod2pi[\[Phi]-t-Pi/2];
	lengthrs=Rmin*(t+Pi/2+v)+ u;
	Return[{lengthrs,t,u,v}];
]; 

c1c2sc21c[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
	a=x+rs;
	b=y-rc;
	u1=Sqrt[a^2+b^2];
	If[Re[u1]<4Rmin,Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	u=Sqrt[u1^2-(4Rmin^2)]-(4Rmin);
	If[Re[u]<0.0, Return[{Infinity,0,0,0}]];
	\[Alpha]=atan[(2Rmin),(u+4Rmin)];
	t=mod2pi[Pi/2+\[Theta]+\[Alpha]];
	v=mod2pi[t-\[Phi]];
	lengthrs=Rmin*(t+Pi+v)+u;
	Return[{lengthrs,t,u,v}];
];

csc21ca[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
	a=x-rs;
	b=y+rc;
	u1=Sqrt[a^2+b^2];
	If[Re[u1]<2Rmin ,Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	u=Sqrt[u1^2-(4Rmin^2)]-2Rmin;
	If[Re[u]<0,Return[{Infinity,0,0,0}]];
	\[Alpha]=atan[(u+2Rmin),(2Rmin)];
	t=mod2pi[Pi/2+\[Theta]-\[Alpha]];
	v=mod2pi[t-Pi/2-\[Phi]];
	lengthrs=Rmin*(t+Pi/2+v)+u;
	Return[{lengthrs,t,u,v}];
]; 

csc21cb[x_,y_,\[Phi]_,rs_,rc_]:=Module[{a,b,u1,\[Theta],\[Alpha],lengthrs,t,u,v},
	a=x+rs;
	b=y-rc;
	u1=Sqrt[a^2+b^2];
	If[Re[u1]<2Rmin,Return[{Infinity,0,0,0}]];
	\[Theta]=atan[b,a];
	t=mod2pi[\[Theta]];
	u=u1-2Rmin;
	v=mod2pi[-t-Pi/2+\[Phi]];
	lengthrs=Rmin*(t+Pi/2+v)+ u;
	Return[{lengthrs,t,u,v}];
];

ReedShepp[{x1_, y1_, t1_}, {x2_, y2_, t2_}]:=Module[{x,y,\[Phi],var,vard,\[Theta],\[Alpha],dx,dy,length, ap,am,b1,b2,t,u,v},
   dx=x2-x1;
   dy=y2-y1;
   \[Theta]=atan[dy,dx];
   \[Alpha]=\[Theta]-t1;
   vard=Norm[{dx,dy}];
   x=vard*Cos[\[Alpha]];
   y=vard*Sin[\[Alpha]];
   \[Phi]=t2-t1;
   ap=Rmin*Sin[\[Phi]];
   am=-Rmin*Sin[\[Phi]];
   b1=Rmin*(Cos[\[Phi]]-1);
   b2=Rmin*(Cos[\[Phi]]+1);
(*\:4f9d\:6b21\:8ba1\:7b9712\:4e2a\:57fa\:5b57\:ff0c\:4eceC|C|C\:5f00\:59cb\:ff0c\:5982\:679c\:6709\:957f\:5ea6\:66f4\:5c0f\:7684\:5c31\:6254\:6389\:5927\:7684*)
{length,t,u,v}=c1c1c[x,y,\[Phi],ap,b1];num=1;RSl1=t;RSl2=u;RSl3=v;
{var,t,u,v}=c1c1c[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=2;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c1c[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=3;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c1c[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=4;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cc[x,y,\[Phi],ap,b1];   If[Re[var]<Re[length],length=var;num=5;RSl1=t;RSl2=u;RSl3=v;];
(*C|CC*)
{var,t,u,v}=c1cc[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=6;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cc[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=7;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cc[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=8;RSl1=t;RSl2=u;RSl3=v;];
(*CSC*)
{var,t,u,v}=csca[x,y,\[Phi],ap,b1];  If[Re[var]<Re[length],length=var;num=9;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csca[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=10;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csca[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=11;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csca[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=12;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cscb[x,y,\[Phi],ap,b2];  If[Re[var]<Re[length],length=var;num=13;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cscb[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=14;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cscb[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=15;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cscb[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=16;RSl1=t;RSl2=u;RSl3=v;];
(*CCu|CuC*)
{var,t,u,v}=ccu1cuc[x,y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=17;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=ccu1cuc[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=18;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=ccu1cuc[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=19;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=ccu1cuc[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=20;RSl1=t;RSl2=u;RSl3=v;];
(*C|Cu Cu|C*)
{var,t,u,v}=c1cucu1c[x,y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=21;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cucu1c[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=22;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cucu1c[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=23;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1cucu1c[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=24;RSl1=t;RSl2=u;RSl3=v;];
(*C|C2 S C*)
{var,t,u,v}=c1c2sca[x,y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=25;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sca[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=26;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sca[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=27;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sca[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=28;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2scb[x,y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=29;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2scb[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=30;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2scb[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=31;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2scb[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=32;RSl1=t;RSl2=u;RSl3=v;];
(*C|C2 S C2|C*)
{var,t,u,v}=c1c2sc21c[x,y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=33;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sc21c[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=34;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sc21c[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=35;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=c1c2sc21c[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=36;RSl1=t;RSl2=u;RSl3=v;];
(*C C|C*)
{var,t,u,v}=cc1c[x,y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=37;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cc1c[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=38;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cc1c[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=39;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=cc1c[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=40;RSl1=t;RSl2=u;RSl3=v;];
(*C S C2|C*)
{var,t,u,v}=csc21ca[x,y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=41;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21ca[x,-y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=42;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21ca[-x,y,-\[Phi],am,b1];If[Re[var]<Re[length],length=var;num=43;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21ca[-x,-y,\[Phi],ap,b1];If[Re[var]<Re[length],length=var;num=44;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21cb[x,y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=45;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21cb[x,-y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=46;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21cb[-x,y,-\[Phi],am,b2];If[Re[var]<Re[length],length=var;num=47;RSl1=t;RSl2=u;RSl3=v;];
{var,t,u,v}=csc21cb[-x,-y,\[Phi],ap,b2];If[Re[var]<Re[length],length=var;num=48;RSl1=t;RSl2=u;RSl3=v;];
Return[length];
];


fctcurve[ty_,orientation_,val_,x1t_,y1t_,t1t_,deltat_,nn_]:=Module[{i,va1,va2,l,newval,incrt,remain,centerx,centery,x2,y2,t2,nnew,m},
	m=nn;delta=deltat;
	If[ty==3, If[Abs[val/Rmin]<EPS, Return[0]],
	If[Abs[val]<EPS , Return[0]]];
	Switch[ty,
	1,(*right arc*)
	{centerx= x1t+Rmin*Sin[t1t];
	centery= y1t-Rmin*Cos[t1t];
	va1= t1t+Pi/2;
	If [orientation==1,va2=va1-val,va2=va1+val];
	x2=centerx+Rmin*Cos[va2];
	y2=centery+Rmin*Sin[va2];
	t2= t1t-orientation*val;
	nnew=Round[val/delta];(*C\:8bed\:8a00\:4e2d\:7684nnew\:58f0\:660e\:4e3aint\:7c7b\:578b*)
	remain=val-nnew*delta;
	nnew=nnew+m;
	If[orientation==-1,delta=-delta];
	incrt=0;
	For[i=m,i<nnew,i++,
	{va1=va1-delta;
	AppendTo[pathx,centerx+Rmin*Cos[va1]];
	AppendTo[pathy,centery+Rmin*Sin[va1]];
	incrt=incrt-delta;
	AppendTo[path\[Theta],mod2pi[t1t+incrt]];
	}];
	m=nnew;
  },
	2,(*left arc*)
	{centerx= x1t-Rmin*Sin[t1t];
	centery= y1t+Rmin*Cos[t1t];
	va1=t1t-Pi/2;
	If[orientation==1, va2=va1+val,va2=va1-val];
	x2=centerx+Rmin*Cos[va2];
	y2=centery+Rmin*Sin[va2];
	t2=t1t+orientation*val;
	nnew=Round[val/delta];
	remain=val-nnew*delta;
	nnew=nnew+m;
	If[orientation==-1,delta=-delta];
	incrt=0;
	For[i=m,i<nnew,i++,
	{va1=va1+delta;
	AppendTo[pathx,centerx+Rmin*Cos[va1]];
	AppendTo[pathy,centery+Rmin*Sin[va1]];
	incrt=incrt+delta;
	AppendTo[path\[Theta],mod2pi[t1t+incrt]];
	} ];
	m=nnew;
	},
	3,(*straight line*)
	{x2=x1t+orientation*val*Cos[t1t];
	y2=y1t+orientation*val*Sin[t1t];
	t2=mod2pi[t1t];
	va1=Sqrt[(x2-x1t)^2+(y2-y1t)^2];
	i=va1/delta;
	remain=va1-i*delta;
	nnew=m+i;
	newval=delta;
	va1=orientation*Cos[t2];
	va2=orientation*Sin[t2];
	For[i=m,i<nnew,i++,
	AppendTo[pathx,x1t+va1*newval];
	AppendTo[pathy,y1t+va2*newval];
	AppendTo[path\[Theta],t2];
	newval=newval+delta;];
	If[remain>0.4,{m=nnew+1;}, {m=nnew;}];
	}];
	Return[{m,x2,y2,t2}];
];
constRS[num_, {RS1_, RS2_, RS3_}, {xs_, ys_, \[Theta]s_}, delta_]:=Module[{m,right,left,straight,fwd,bwd,x1,y1,t1,RSl1,RSl2,RSl3},
    m=1;right=1;left=2;straight=3;fwd=1;bwd=-1;
    x1=xs;y1=ys;t1=\[Theta]s;
    If[RS1<EPS,RSl1=1/EPS,RSl1=RS1];
    If[RS2<EPS,RSl2=1/EPS,RSl2=RS2];
    If[RS3<EPS,RSl3=1/EPS,RSl3=RS3];
    AppendTo[pathx,x1];AppendTo[pathy,y1];AppendTo[path\[Theta],t1];
    Switch[num,
    (*C|C|C*)
    1,
    {m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
    {m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
    {m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
    2,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	3,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	4,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	(*C|C C*)
	5,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	6,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	7,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	8,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	(*C S C*)
	9,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	10,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	11,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	12,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	13,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	14,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	15,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	16,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	(*C Cu|Cu C*)
	17,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	18,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	19,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	20,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	(*C|Cu Cu|C*)
	21,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	22,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	23,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	24,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	(*C|C2 S C*)
	25,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	26,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	27,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	28,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	29,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	30,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	31,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	32,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	(*C|C2 S C2|C*)
	33,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	34,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	35,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	36,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	(*C C|C*)
	37,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	38,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	39,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	40,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	(*C S C2|C*)
	41,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	42,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	43,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];,
	44,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	45,
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl3,x1,y1,t1,delta,m];,
	46,
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,fwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl3,x1,y1,t1,delta,m];,
	47,
	{m,x1,y1,t1}=fctcurve[left,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,fwd,RSl3,x1,y1,t1,delta,m];,
	48,
	{m,x1,y1,t1}=fctcurve[right,bwd,RSl1,x1,y1,t1,delta,1];
	{m,x1,y1,t1}=fctcurve[straight,bwd,RSl2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[right,bwd,Pi/2,x1,y1,t1,delta,m];
	{m,x1,y1,t1}=fctcurve[left,fwd,RSl3,x1,y1,t1,delta,m];
	]
];


(*End[];*)
EndPackage[];
