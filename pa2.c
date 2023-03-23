// ----------------------------------------------------------------
// 
//   4190.308 Computer Architecture (Fall 2022)
// 
//   Project #2: SFP16 (16-bit floating point) Adder
// 
//   October 4, 2022
// 
//   Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
//   Jaehoon Shim (mattjs@snu.ac.kr)
//   IlKueon Kang (kangilkueon@snu.ac.kr)
//   Wookje Han (gksdnrwp@snu.ac.kr)
//   Jinsol Park (jinsolpark@snu.ac.kr)
//   Systems Software & Architecture Laboratory
//   Dept. of Computer Science and Engineering
//   Seoul National University
// 
// ----------------------------------------------------------------

#define bias 63
#define N (SFP16)0xff01
#define posinf (SFP16)0x7f00
#define neginf (SFP16)0xff00

typedef unsigned short SFP16;

unsigned char checking(SFP16 val);
SFP16 sum(SFP16 x, SFP16 y);
SFP16 sub(SFP16 x, SFP16 y);
void norm(SFP16* val,char* Eval);
void rounding(SFP16* val);
void normlast(SFP16* val,char* Eval);
void encode(SFP16* val, char Eval, unsigned char finalsign);

SFP16 fpadd(SFP16 x,SFP16 y){
  SFP16 res = 0;
  //swapping
  if((x|(SFP16)(1<<15))<(y|(SFP16)(1<<15))) return fpadd(y,x);

  //calculate d
  char Ex=(char)(x>>8) & (char)0x7f; //x&01111111 makes sign bit 0
  char Ey=(char)(y>>8) & (char)0x7f;
  unsigned char signx=x>>15, signy=y>>15;
  unsigned char state_x=checking(x), state_y=checking(y);
  unsigned char leading_Mx,leading_My;

  if(state_x==3) return x; 
  else if(state_y==3) return y; //return nan

  if(state_x==2){
    if(state_y==2){
      if((signx^signy)==1) {return N;} //both are inf but signs are different
      else return x; //both are inf with same signs
    }
    else return x;
  }

  if(state_y==2) return y;
  if(state_x==0) return y;
  if(state_y==0) return x;
  //now only nonzero denorm/norm values are left

  if(state_x==1) {Ex=1-bias; leading_Mx=0;}
  else {Ex-=bias; leading_Mx=1;}
  if(state_y==1) {Ey=1-bias; leading_My=0;}
  else {Ey-=bias; leading_My=1;}
  //calculate E values

  unsigned char g_x=0,r_x=0,s_x=0;
  unsigned char g_y=0,r_y=0,s_y=0;
  unsigned char Mx=(unsigned char)x;
  unsigned char My=(unsigned char)y;

  for(char i=0;i<(Ex-Ey);i++){
    if(i==0){
      g_y=My&(unsigned char)1;
      if(state_y==5) {
        My=(My>>1) | (unsigned char)(1<<7);
        leading_My=0;
        }
      else My=(My>>1);
    }
    else{
      s_y=r_y|s_y;
      r_y=g_y;
      g_y=My&((unsigned char)1);
      My>>=1;
    }
  }

  SFP16 numx=0,numy=0;
  numx=((SFP16)leading_Mx<<11)+((SFP16)Mx<<3)+((SFP16)g_x<<2)+((SFP16)r_x<<1) \
  +((SFP16)s_x);
  numy=((SFP16)leading_My<<11)+((SFP16)My<<3)+((SFP16)g_y<<2)+((SFP16)r_y<<1) \
  +((SFP16)s_y);

  unsigned char finalsign;

  if(signx==signy){
    res=sum(numx,numy);
    finalsign=signx;
  }
  else {
    res=sub(numx,numy);
    finalsign=signx;
  }
  norm(&res,&Ex);
  rounding(&res);
  normlast(&res,&Ex);
  encode(&res,Ex,finalsign);
  return res;
}

unsigned char checking(SFP16 val){
  unsigned char Eval=(unsigned char)(val>>8) & (unsigned char)0x7f;
  if(val==0 || val==0x8000) return (unsigned char)0; //zero
  if(Eval==0x7f && (unsigned char)val==0) return (unsigned char)2; //inf
  if(Eval==0x7f && (unsigned char)val==1) return (unsigned char)3; //nan
  if(Eval==0) return (unsigned char)1; //denorm
  else return (unsigned char)5; //norm
}

SFP16 sum(SFP16 x, SFP16 y){
  SFP16 res=x+y;
  return res;
}

SFP16 sub(SFP16 x, SFP16 y){
 SFP16 res=x-y;
  return res;
}

void norm(SFP16* val,char* Eval){
  SFP16 s;
  if(((*val>>12)&1)==1){
    s=*val&1;
    *val>>=1;
    *val=*val|s;
    *Eval+=1;
    return;
  }
  if(*val==0){return;}
  while(((*val>>11)&1)==0 && (*Eval)>-62){
    *val<<=1;
    *Eval-=1;
  }
}

void rounding(SFP16* val){
  SFP16 s,R,S;
  s=*val&1;
  *val>>=1;
  *val=*val|s;
  R=(*val&(SFP16)2)>>1;
  S=*val&1;
  if(R==1){
    if(S==1){
      *val>>=2;
      *val+=1;
    }
    else{
      *val>>=2;
      if((*val&1)==1) *val+=1;
    }
  }
  else *val>>=2;
}

void normlast(SFP16* val,char* Eval){
  SFP16 s;
  if(((*val>>9)&1)==1){
    s=*val&1;
    if(s==1){
      *val>>=1;
      (*Eval)++;
      if((*val&1)==1){
        *val+=1;
      }
    }
    else {
      *val>>=1;
      (*Eval)++;
    }
  }
}

void encode(SFP16* val, char Eval, unsigned char finalsign){
  if(Eval>63) {
    if(finalsign==0){*val=posinf;}
    else *val=neginf;
    return;
  }
  else if(Eval<-62){
    *val=0;
    return;
  }
  else if(Eval==-62){
    char check=0;
    if((*val>>8)==1) check=1;
    *val=*val&0xff;
    *val|=((SFP16)finalsign)<<15;
    if(check==1) *val|=((SFP16)(Eval+bias))<<8;
    return;
  }
  *val=*val&0xff;
  *val|=((SFP16)finalsign)<<15;
  *val|=((SFP16)(Eval+bias))<<8;
}