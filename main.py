import json,os,urllib.request as u
U=os.getenv("MAKEFUN_ENV_URL")
def P(p,b):
 return json.load(u.urlopen(U+p,json.dumps(b).encode()))
V=[0,100,320,330,500,900,0]
N=[-33,-31,-18,-14,14,18,31,33]
D=[-17,-16,-15,-1,1,15,16,17]
A=N+D
B=[0]*256;T=1;CS=0;EP=-1;ST=[];ND=0;X=Exception;KW=0;KB=0;J=[N,(-17,-15,15,17),(-16,-1,1,16),D,D]
def F(s):
 global T,CS,EP,KW,KB
 B[:]=[0]*256
 p=s.split();c=112
 for ch in p[0]:
  if ch=="/":c-=24
  elif ch.isdigit():c+=int(ch)
  else:
   v="PNBRQK".index(ch.upper())+1
   B[c]=v if ch.isupper() else-v;c+=1
 T=1 if p[1]=="w" else-1
 CS=sum(1<<i for i in range(4)if"KQkq"[i]in p[2])
 EP=-1 if p[3]=="-" else(int(p[3][1])-1)*16+ord(p[3][0])-97
 KW=B.index(6);KB=B.index(-6)
def AT(s,y):
 for d in A+[-15,-17,15,17]:
  e=s+d
  if not e&136:
   q=B[e]
   if q==2*y and d in N or q==6*y and d in D or q==y and d*y in(-15,-17):return 1
 for d in D:
  x=s+d
  while not x&136:
   q=B[x]
   if q:
    if q==5*y or q==(4*y if abs(d)in(1,16) else 3*y):return 1
    break
   x+=d
 return 0
def G():
 m=[]
 for s in range(128):
  p=B[s]
  if s&136 or p*T<=0:continue
  a=abs(p)
  if a==1:
   d=16*T;f=s+d
   if not B[f]:
    z=(5<<14,)if f>>4 in(0,7)else(0,)
    m+=[s|f<<7|q for q in z]
    if(s>>4)==(1 if T>0 else 6)and not B[s+2*d]:m.append(s|(s+2*d)<<7)
   for e in(f-1,f+1):
    if e&136:continue
    q=B[e]
    if q*T<0:
     z=(5<<14,)if e>>4 in(0,7)else(0,)
     m+=[s|e<<7|q for q in z]
    elif e==EP:m.append(s|e<<7)
  else:
   for d in J[a-2]:
    e=s+d
    if a in(2,6):
     if not e&136 and B[e]*T<=0:m.append(s|e<<7)
    else:
     while not e&136 and not B[e]:m.append(s|e<<7);e+=d
     if not e&136 and B[e]*T<0:m.append(s|e<<7)
 for k,z,s in((4,1,1),(4,2,-1),(116,4,1),(116,8,-1)):
  t=k+2*s
  if CS&z and not(B[k+s]or B[t]) and(s>0 or not B[k+3*s])and not(AT(k,-T)or AT(k+s,-T)):m.append(k|t<<7)
 return m
def MK(m):
 global T,CS,EP,KW,KB
 f=m&127;t=(m>>7)&127;q=m>>14
 p=B[f];v=B[t]
 ST.append((m,v,CS,EP,KW,KB,p))
 B[t]=q or p;B[f]=0
 if abs(p)==1 and t==EP and not v:B[t-16*T]=0
 EP=(f+t)//2 if abs(p)==1 and abs(t-f)==32 else-1
 if abs(p)==6:
  if T>0:KW=t
  else:KB=t
  if abs(t-f)==2:
   rf,rt=(t+1,t-1)if t>f else(t-2,t+1)
   B[rt]=B[rf];B[rf]=0
 for s in(f,t):CS&=~{4:3,7:1,0:2,116:12,119:4,112:8}.get(s,0)
 T=-T
def UM():
 global T,CS,EP,KW,KB
 m,v,c,e,kw,kb,p=ST.pop()
 f=m&127;t=(m>>7)&127;q=m>>14
 B[f]=p;B[t]=v
 if abs(p)==1 and t==e and not v:B[t+16*T]=T
 if abs(p)==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1)
  B[rf]=B[rt];B[rt]=0
 T,CS,EP,KW,KB=-T,c,e,kw,kb
def O(m):
 return V[abs(B[m>>7&127])]*10
def Q(a,be):
 global ND
 ND+=1
 if ND>1e5:raise X
 s=0
 for q in range(128):
  p=B[q]
  if p and q&136==0:
   w=abs(p);v=V[w]
   if w==1:v+=(7-(q>>4)if p<0 else q>>4)
   if w in(2,3):v+=(6-abs((q&7)-3)-abs((q>>4)-3))*4
   if w>5:v+=8*(B[q+16*T]==T)
   s+=v if p*T>0 else-v
 if s>=be:return be
 a=max(a,s)
 ms=sorted([m for m in G() if B[m>>7&127] or m>>14],key=O,reverse=True)
 for m in ms:
  MK(m)
  if AT(KW if T<0 else KB,T):UM();continue
  s=-Q(-be,-a)
  UM()
  a=max(a,s)
  if a>=be:break
 return a
def S(d,a,be):
 global ND
 ND+=1
 if ND>1e5:raise X
 if d==0:return Q(a,be)
 ms=sorted(G(),key=O,reverse=True)
 l=0
 for m in ms:
  MK(m)
  if AT(KW if T<0 else KB,T):UM();continue
  l=1
  s=-S(d-1,-be,-a)
  UM()
  a=max(a,s)
  if a>=be:break
 if not l:return-99999 if AT((KB,KW)[T>0],-T)else 0
 return a
def R(ms):
 global ND
 bm=ms[0]
 for d in range(9):
  try:
   ND=0;sc=[]
   for m in ms:
    MK(m)
    if AT(KW if T<0 else KB,T):UM();continue
    s=-S(d,-1e9,1e9)
    UM();sc.append((s,m))
   sc.sort(reverse=True);bm=sc[0][1];ms=[x[1]for x in sc]
  except X:
   break
 return bm
def C(u):
 return((int(u[1])-1)*16+ord(u[0])-97)|((int(u[3])-1)*16+ord(u[2])-97)<<7|{"q":5,"n":2,"r":4,"b":3}.get(u[4:],0)<<14
def W(m):
 return chr(97+(m&7))+str((m&127)//16+1)+chr(97+((m>>7)&7))+str(((m>>7)&127)//16+1)+{5:"q",2:"n",4:"r",3:"b"}.get(m>>14,"")
while(g:=P("/game",{}))["status"]>"e":
 while g["status"]=="your_move":
  F(g["fen"])
  bm=R([C(u) for u in g["legal_moves"]])
  g=P("/move",{"game":g["game"],"move":W(bm)})