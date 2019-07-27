(* ::Package:: *)

(*\:8ba1\:7b97Dubins\:66f2\:7ebf\:5e76\:751f\:6210\:8def\:5f84\:4e0a\:7684\:79bb\:6563\:70b9*) 


BeginPackage["Dubins`"]; 
(*Rmin=Global`Rmin;*)(*\:6700\:5c0f\:8f6c\:5411\:534a\:5f84Rmin\:5b9a\:4e49\:6210\:5168\:5c40\:53d8\:91cf*)
INFINITY=10^10;
LSL=0;
LSR=1;
RSL=2;
RSR=3;
RLR=4;
LRL=5;
(*The three segment types a path can be made up of*)
LSEG=0;
SSEG=1;
RSEG =2;
EDUBOK =0;(*No error*)
EDUBCOCONFIGS =1;(*Colocated configurations*)
EDUBPARAM =2;(*Path parameterisitation error*)
EDUBBADRHO=3;(*the rho value is invalid*)
EDUBNOPATH=4;(*no connection between configurations with this word*)
(*The segment types For each of the Path types*)
DIRDATA={{LSEG,SSEG,LSEG},{LSEG,SSEG,RSEG},{RSEG,SSEG,LSEG},{RSEG,SSEG,RSEG},{RSEG,LSEG,RSEG},{LSEG,RSEG,LSEG}};
DubinsWords={DubinsLSL,DubinsLSR,DubinsRSL,DubinsRSR,DubinsRLR,DubinsLRL};
mod2pi::usage ="\:5c06\:89d2\:5ea6\:8f6c\:6362\:4e3a0~2pi\:8303\:56f4\:5185"
atan::usage ="\:53cd\:6b63\:5207\:ff0c\:8003\:8651\:4e86\:70b9\:5728\:539f\:70b9\:7684\:7279\:4f8b"
DubinsInitNormalised::usage ="\:7a77\:4e3e\:6240\:6709\:7c7b\:578b"
DubinsInit::usage =" "
DubinsLSL::usage =" "
DubinsRSR::usage =" "
DubinsLSR::usage =" "
DubinsRSL::usage =" "
DubinsRLR::usage =" "
DubinsLRL::usage =" "
DubinsSegment::usage =" "
DubinsPathLength::usage =" "
DubinsPathSample::usage =" "
DubinsPathSampleMany::usage =" "


fmodr[x_,y_]:=x-y*Floor[x/y];
mod2pi[theta_]:=fmodr[theta,2.0*Pi];
atan[y_,x_]:=mod2pi@If[{x,y}=={0,0},0,ArcTan[x,y]]


DubinsInitNormalised[alpha_,beta_,d_]:=Module[{bestcost=INFINITY,bestword,i,cost,errout,outputs,param,type},
(*\:8d1f\:8d23\:8ba1\:7b97\:51fa\:6700\:4f18\:7684Dubins\:66f2\:7ebf*)
bestword=-1;
   For[i=1,i<=6,i++,
       {errout,outputs}=DubinsWords[[i]][alpha,beta,d];
       If[errout==EDUBOK,
         cost=Total[outputs];
         If[cost<bestcost,
         bestword=i;
         bestcost=cost;
         param=outputs;
         type=i;
         ];
        ]
     ];
	If[bestword==-1,Return[{EDUBNOPATH,{}}]];
	type=bestword;
	Return[{EDUBOK,param,type}];
]

DubinsInit[q0_,q1_]:=Module[{dx,dy,dD,alpha,beta,d,null,theta},
	dx=q1[[1]]-q0[[1]];
	dy=q1[[2]]-q0[[2]];
	dD=Sqrt[dx*dx+dy*dy];
	d=dD/Rmin;
	If[Rmin<=0,Return[EDUBBADRHO]];
	theta=mod2pi[atan[dy,dx]];
	alpha=mod2pi[q0[[3]]-theta];
	beta=mod2pi[q1[[3]]-theta];
	{null,param,type}=DubinsInitNormalised[alpha,beta,d];
	Return[{param,type}];
]

DubinsLSL[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,t,p,q,outputs,tmp1},
	sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	tmp0=d+sa-sb;
	psquared=2+d^2-(2*cab)+(2*d*(sa-sb));
	If[psquared<0,Return[{EDUBNOPATH,{}}]];tmp1=atan[cb-ca,tmp0];
	t=mod2pi[-alpha+tmp1];
	p=Sqrt[psquared];
	q=mod2pi[beta-tmp1];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsRSR[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,t,p,q,outputs,tmp1},
	sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	tmp0=d-sa+sb;
	psquared=2+(d*d)-(2*cab)+(2*d*(sb-sa));
	If[psquared<0,Return[{EDUBNOPATH,{}}]];
	tmp1=atan[ca-cb,tmp0];
	t=mod2pi[alpha-tmp1];
	p=Sqrt[psquared];
	q=mod2pi[-beta+tmp1];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsLSR[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,t,p,q,outputs,tmp2},
	sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	psquared=-2+(d*d)+(2*cab)+(2*d*(sa+sb));
	If[psquared<0,Return[{EDUBNOPATH,{}}]]; 
	 p=Sqrt[psquared];
	tmp2=atan[(-ca-cb),(d+sa+sb)]-atan[-2.0,p];
	t=mod2pi[-alpha+tmp2];
	q=mod2pi[-mod2pi[beta]+tmp2];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsRSL[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,t,p,q,outputs,tmp2},
	sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	psquared=(d*d)-2+(2*cab)-(2*d*(sa+sb));
	If[psquared<0,Return[{EDUBNOPATH,{}}]];
	p=Sqrt[psquared];
	tmp2=atan[(ca+cb),(d-sa-sb)]-atan[2.0,p];
	t=mod2pi[alpha-tmp2];
	q=mod2pi[beta-tmp2];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsRLR[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,tmprlr,t,p,q,outputs},
	sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	tmprlr=(6-d*d+2*cab+2*d*(sa-sb))/8.;
	If[Abs[tmprlr]>1,Return[{EDUBNOPATH,{}}]];
	 p=mod2pi[2*Pi-ArcCos[tmprlr]];
	t=mod2pi[alpha-atan[ca-cb,d-sa+sb]+mod2pi[p/2.]];
	q=mod2pi[alpha-beta-t+mod2pi[p]];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsLRL[alpha_,beta_,d_]:=Module[{sa,sb,ca,cb,cab,tmp0,psquared,tmplrl,t,p,q,outputs},sa=Sin[alpha];
	sb=Sin[beta];
	ca=Cos[alpha];
	cb=Cos[beta];
	cab=Cos[alpha-beta];
	tmplrl=(6.-d*d+2*cab+2*d*(-sa+sb))/8.;
	If[Abs[tmplrl]>1,Return[{EDUBNOPATH,{}}]];
	p=mod2pi[2*Pi-ArcCos[tmplrl]];
	t=mod2pi[-alpha-atan[ca-cb,d+sa-sb]+p/2.];
	q=mod2pi[mod2pi[beta]-alpha-t+mod2pi[p]];
	outputs=Re[{t,p,q}];
	Return[{EDUBOK,outputs}];
]

DubinsSegment[t_,qi_,type_]:=Module[{qttmp},
	qttmp={0,0,0};
	If[type==LSEG,
	qttmp[[1]]=qi[[1]]+Sin[qi[[3]]+t]-Sin[qi[[3]]];
	qttmp[[2]]=qi[[2]]-Cos[qi[[3]]+t]+Cos[qi[[3]]];
	qttmp[[3]]=qi[[3]]+t];
	 If[type==RSEG,
	qttmp[[1]]=qi[[1]]-Sin[qi[[3]]-t]+Sin[qi[[3]]];
	qttmp[[2]]=qi[[2]]+Cos[qi[[3]]-t]-Cos[qi[[3]]];
	qttmp[[3]]=qi[[3]]-t]; 
	If[type==SSEG,
	qttmp[[1]]=qi[[1]]+Cos[qi[[3]]]*t;
	qttmp[[2]]=qi[[2]]+Sin[qi[[3]]]*t;
	qttmp[[3]]=qi[[3]]];
	Return[qttmp]
]

DubinsPathLength[param_]:=Total[param]*Rmin;

DubinsPathSample[t_,qi_,param_,type_]:=Module[{qitmp,p1,p2,tprime,types,q,q1,q2},
	tprime=t/Rmin;(*tprime is the normalised variant of the parameter t*)
	qitmp={0,0,qi[[3]]};
	(*Generate the target configuration*)
	types=DIRDATA[[type]];
	p1=param[[1]];
	p2=param[[2]];
	q1=DubinsSegment[p1,qitmp,types[[1]]];
	q2=DubinsSegment[p2,q1,types[[2]]];
	If[tprime<p1,
		q=DubinsSegment[tprime,qitmp,types[[1]]];
		,
		If[tprime<(p1+p2),
		q=DubinsSegment[tprime-p1,q1,types[[2]]],
		q=DubinsSegment[tprime-p1-p2,q2,types[[3]]];
		]
	]; 
	q={Rmin*q[[1]]+qi[[1]],Rmin*q[[2]]+qi[[2]],mod2pi[q[[3]]]};
	q
]

DubinsPathSampleMany[q0_,q1_,stepSize_]:=Module[{param,type,length,q,qs={},t=0},
	{param,type}=DubinsInit[q0,q1];
	length=DubinsPathLength[param];
	While[t<length,
	q=DubinsPathSample[t,q0,param,type];
	AppendTo[qs,q];
	t+=stepSize];
	qs
]


EndPackage[];
