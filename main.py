import json,os,urllib.request as r
def h(p,d):
 return json.load(r.urlopen(os.getenv("MAKEFUN_ENV_URL","")+p,json.dumps(d).encode()))
vl=(0,100,320,330,500,900,0)
kj=(-33,-31,-18,-14,14,18,31,33)
dg=(-17,-15,15,17,-16,-1,1,16)
al=kj+dg
bd=[0]*256;sk=[];E=IOError;tt={}
jt=[kj,dg[:4],dg[4:],dg,dg]
def fs(s):
 global sd,cs,ep,wk,bk
 bd[:]=[0]*256
 p=s.split();c=112
 for ch in p[0]:
  if ch=="/":c-=24
  elif ch.isdigit():c+=int(ch)
  else:
   v="PNBRQK".index(ch.upper())+1
   bd[c]=v if ch.isupper() else-v;c+=1
 sd=1 if p[1]=="w" else-1
 cs=sum(1<<i for i in range(4)if"KQkq"[i]in p[2])
 ep=-1 if p[3]=="-" else sq2(p[3])
 wk=bd.index(6);bk=bd.index(-6)
def ak(s,y):
 for d in al:
  e=s+d
  if not e&136:
   q=bd[e]
   if q==2*y and d in kj or q==6*y and d in dg or q==y and d*y in(-15,-17):return 1
 for d in dg:
  x=s+d
  while not x&136:
   q=bd[x]
   if q:
    if q==5*y or q==(4*y if abs(d)in(1,16) else 3*y):return 1
    break
   x+=d
 return 0
def ck():return ak((bk,wk)[sd<0],sd)
def mg():
 m=[]
 for s in range(128):
  p=bd[s]
  if s&136 or p*sd<=0:continue
  a=abs(p)
  if a==1:
   d=16*sd;f=s+d
   for e in(f-1,f,f+1):
    if e&136 or abs((e&7)-(f&7))>1:continue
    q=bd[e]
    if e==f:
     if q:continue
    elif q*sd>=0 and e!=ep:continue
    z=(81920,)if e>>4 in(0,7)else(0,)
    m+=[s|e<<7|x for x in z]
   if(s>>4)==1+5*(sd<0)and bd[f]==bd[s+2*d]==0:m.append(s|(s+2*d)<<7)
  else:
   for d in jt[a-2]:
    e=s+d
    while not e&136 and not bd[e]:
     m.append(s|e<<7)
     if a in(2,6):break
     e+=d
    if not e&136 and bd[e]*sd<0:m.append(s|e<<7)
 for k,z,s in((4,1,1),(4,2,-1),(116,4,1),(116,8,-1)):
  t=k+2*s
  if cs&z and not(bd[k+s]|bd[t]|(s<0 and bd[k+3*s]))and ak(k,-sd)+ak(k+s,-sd)==0:m.append(k|t<<7)
 return m
def mk(m):
 global sd,cs,ep,wk,bk
 f=m&127;t=m>>7&127;q=m>>14
 p=bd[f];v=bd[t]
 sk.append((m,v,cs,ep,wk,bk,p))
 bd[t]=q or p;bd[f]=0
 if abs(p)==1 and t==ep and not v:bd[t-16*sd]=0
 ep=(f+t)//2 if abs(p)==1 and abs(t-f)==32 else-1
 if abs(p)==6:
  if sd>0:wk=t
  else:bk=t
  if abs(t-f)==2:
   rf,rt=(t+1,t-1)if t>f else(t-2,t+1)
   bd[rt]=bd[rf];bd[rf]=0
 for s in(f,t):cs&=~{4:3,7:1,0:2,116:12,119:4,112:8}.get(s,0)
 sd=-sd
def um():
 global sd,cs,ep,wk,bk
 m,v,c,e,kw,kb,p=sk.pop()
 f=m&127;t=m>>7&127
 bd[f]=p;bd[t]=v
 if abs(p)==1 and t==e and not v:bd[t+16*sd]=sd
 if abs(p)==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1)
  bd[rf]=bd[rt];bd[rt]=0
 sd,cs,ep,wk,bk=-sd,c,e,kw,kb
def ov(m):
 return vl[abs(bd[m>>7&127])]
def qs(a,b):
 global nn
 nn+=1
 if nn>1e5:raise E
 s=0
 for q in range(128):
  p=bd[q]
  if p and not q&136:
   w=abs(p);v=vl[w];f=q&7;r=q>>4;c=6-abs(f-3)-abs(r-3)
   if w==1:v+=r if p>0 else 7-r
   elif w in(2,3):v+=c*4
   elif w>3:v+=c*(2 if w<6 else-3)
   if w==6 and bd[q+16*(p//6)]==p//6:v+=8
   s+=v if p*sd>0 else-v
 a=max(a,s)
 if a>=b:return b
 for m in sorted([m for m in mg() if bd[m>>7&127]|m>>14],key=ov,reverse=1):
  mk(m)
  if ck():um();continue
  s=-qs(-b,-a)
  um()
  a=max(a,s)
  if a>=b:return b
 return a
def sc(d,a,b):
 global nn
 nn+=1
 if nn>1e5:raise E
 k=tuple(bd[:128])+(sd,)
 e=tt.get(k,(-1,0,0))
 if e[0]>=d and not a<e[1]<b:return e[1]
 if d==0:return qs(a,b)
 ms=mg();ms.sort(key=lambda m:(m!=e[2],-ov(m)))
 bm=l=0
 for m in ms:
  mk(m)
  if ck():um();continue
  l=1
  s=-sc(d-1,-b,-a)
  um()
  if s>=b:
   tt[k]=(d,s,m);return b
  if s>a:a=s;bm=m
 if not l:return-9999 if ak((bk,wk)[sd>0],-sd) else 0
 tt[k]=(d,a,bm)
 return a
def rt(ms):
 global nn
 bm=ms[0]
 for d in range(9):
  try:
   nn=0;v=[]
   for m in ms:
    mk(m)
    if ck():um();continue
    s=-sc(d,-9999,9999)
    um();v+=[(s,m)]
   v.sort(reverse=1);bm=v[0][1];ms=[x[1]for x in v]
  except E:
   break
 return bm
def sq2(s):return(int(s[1])-1)*16+ord(s[0])-97
def cu(u):return sq2(u[:2])|sq2(u[2:])<<7|"  nbrq".index(u[4:])<<14
def sq(x):return chr(97+(x&7))+str((x&127)//16+1)
def mu(m):return sq(m)+sq(m>>7)+"  nbrq"[m>>14].strip()
while(g:=h("/game",{}))["status"]>"e":
 while g["status"]=="your_move":
  fs(g["fen"])
  bm=rt([cu(u) for u in g["legal_moves"]])
  g=h("/move",{"game":g["game"],"move":mu(bm)})