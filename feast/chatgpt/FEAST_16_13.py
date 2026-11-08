"""Generalized unconstrained-master FEAST optimization for even and odd n."""
import os
os.environ['OPENBLAS_NUM_THREADS']='1'
import numpy as np
from scipy.interpolate import CubicSpline
from scipy.optimize import minimize
from pathlib import Path

OUT=Path(__file__).resolve().parent
W=dict(home_rad=.08,home_tan=.50,move=.23,accel=1.60,under6=150.,under9=0.,over10=3.,over12=100.,collision=85.,cross=90.)
class Feast:
 def __init__(self,n):
  self.n=n; self.R=7/(2*np.sin(np.pi/n));self.BASE=np.tile([self.R,0.],(n,1))
  a=2*np.pi*np.arange(n)/n
  self.ROT=np.array([[[np.cos(v),np.sin(v)],[-np.sin(v),np.cos(v)]] for v in a])
  self.PI,self.PJ,self.PT=map(np.array,zip(*[(i,j,t) for t in range(n) for i in range(n) for j in [(t-i)%n] if i<j]))
  self.A,self.B=np.triu_indices(n,1)
  self.S=4; self.C=self.interp(np.arange(n*self.S)/self.S);self.CB=self.interp(np.arange(n))
  self.tri=np.array([(t,i,j,k) for t,i,j in zip(self.PT,self.PI,self.PJ) for k in range(n) if k!=i and k!=j],dtype=int)
  self.TT,self.II,self.JJ,self.KK=self.tri.T
 def interp(self,times):
  basis=np.eye(self.n)
  sp=CubicSpline(np.arange(self.n+1),np.vstack([basis,basis[:1]]),axis=0,bc_type='periodic')
  return sp((np.asarray(times)[:,None]-2*np.arange(self.n)[None,:])%self.n)
 def pos(self,q,C): return np.einsum('icd,sid->sic',self.ROT,np.einsum('sik,kd->sid',C,q,optimize=True),optimize=True)
 def back(self,gg,C):return np.einsum('sik,sid->kd',C,np.einsum('icd,sic->sid',self.ROT,gg,optimize=True),optimize=True)
 def objective(self,z):
  n=self.n;q=z.reshape(n,2)+self.BASE;dq=q-self.BASE;g=np.zeros_like(q);w=W
  val=w['home_rad']*np.sum(dq[:,0]**2)+w['home_tan']*np.sum(dq[:,1]**2)
  g[:,0]=2*w['home_rad']*dq[:,0];g[:,1]=2*w['home_tan']*dq[:,1]
  d=q-np.roll(q,1,axis=0)
  val+=w['move']*np.sum(d*d);g+=2*w['move']*(2*q-np.roll(q,1,axis=0)-np.roll(q,-1,axis=0))
  acc=np.roll(q,-1,axis=0)-2*q+np.roll(q,1,axis=0)
  val+=w['accel']*np.sum(acc*acc);g+=2*w['accel']*(np.roll(acc,1,axis=0)-2*acc+np.roll(acc,-1,axis=0))
  p=self.pos(q,self.CB);dp=p[self.PT,self.PI]-p[self.PT,self.PJ]; dist=np.maximum(np.linalg.norm(dp,axis=1),1e-8)
  lo6=np.maximum(6-dist,0);lo9=np.maximum(9-dist,0);hi10=np.maximum(dist-10,0);hi12=np.maximum(dist-12,0)
  val+=w['under6']*np.sum(lo6**2)+w['under9']*np.sum(lo9**2)+w['over10']*np.sum(hi10**2)+w['over12']*np.sum(hi12**2)
  coef=(-2*w['under6']*lo6-2*w['under9']*lo9+2*w['over10']*hi10+2*w['over12']*hi12)/dist
  gp=np.zeros_like(p);gg=coef[:,None]*dp
  np.add.at(gp,(self.PT,self.PI),gg);np.add.at(gp,(self.PT,self.PJ),-gg)
  # penalty for a straight pass traversing another juggler's personal disc
  aa=p[self.TT,self.II];bb=p[self.TT,self.JJ];cc=p[self.TT,self.KK]
  vv=bb-aa;vv2=np.maximum(np.sum(vv*vv,axis=1),1e-8)
  ss=np.clip(np.sum((cc-aa)*vv,axis=1)/vv2,0,1)
  residual=cc-aa-ss[:,None]*vv;dby=np.maximum(np.linalg.norm(residual,axis=1),1e-8)
  fail=np.maximum(3.12-dby,0);val+=w['cross']*np.sum(fail*fail)
  gg_by=(-2*w['cross']*fail/dby)[:,None]*residual
  np.add.at(gp,(self.TT,self.KK),gg_by)
  np.add.at(gp,(self.TT,self.II),-(1-ss)[:,None]*gg_by)
  np.add.at(gp,(self.TT,self.JJ),-ss[:,None]*gg_by)
  g+=self.back(gp,self.CB)
  paths=self.pos(q,self.C);diff=paths[:,self.A]-paths[:,self.B];dd=np.maximum(np.linalg.norm(diff,axis=-1),1e-8);bad=np.maximum(6.18-dd,0)
  val+=w['collision']/self.S*np.sum(bad*bad)
  gg=(-2*w['collision']/self.S*bad/dd)[:,:,None]*diff
  gs=np.zeros_like(paths);times=np.arange(len(self.C))[:,None]
  np.add.at(gs,(times,self.A[None,:]),gg);np.add.at(gs,(times,self.B[None,:]),-gg)
  g+=self.back(gs,self.C)
  return float(val),g.ravel()
 def measure(self,q):
  n=self.n;b=self.pos(q,self.CB)
  dist=np.linalg.norm(b[self.PT,self.PI]-b[self.PT,self.PJ],axis=1)
  pp=self.pos(q,self.interp(np.arange(n*40)/40))
  sep=np.linalg.norm(pp[:,self.A]-pp[:,self.B],axis=-1)
  aa=b[self.TT,self.II];bb=b[self.TT,self.JJ];cc=b[self.TT,self.KK];v=bb-aa
  proj=np.clip(np.sum((cc-aa)*v,axis=1)/np.sum(v*v,axis=1),0,1)
  db=np.linalg.norm(cc-aa-proj[:,None]*v,axis=1)
  sp=CubicSpline(np.arange(n+1),np.vstack([q,q[:1]]),axis=0,bc_type='periodic')
  tt=np.arange(n*100)/100;pts=sp(tt)
  walk=np.linalg.norm(np.roll(pts,-1,axis=0)-pts,axis=1).sum()
  rms=np.sqrt(np.mean(np.sum(sp(tt,2)**2,axis=1)))
  return dict(min_throw=float(dist.min()),mean_throw=float(dist.mean()),max_throw=float(dist.max()),walk=float(walk),min_sep=float(sep.min()),min_bystander=float(db.min()),blocked=int(np.sum(db.reshape(-1,n-2).min(axis=1)<3)),rms_acc=float(rms),passes=len(dist),phantoms=n*n-2*len(dist))
 def guess(self,seed):
  rng=np.random.default_rng(seed); k=np.arange(self.n)
  u=-2.8*np.cos(2*np.pi*k/self.n)-1.2*np.cos(4*np.pi*k/self.n)+.6*np.cos(6*np.pi*k/self.n)
  v=4.5*np.sin(2*np.pi*k/self.n)+2.8*np.sin(4*np.pi*k/self.n)
  return np.stack([u,v],axis=1)+rng.normal(0,.5,(self.n,2))
 def solve(self):
  candidates=[]
  for seed in (3,19):
   res=minimize(self.objective,self.guess(seed).ravel(),method='L-BFGS-B',jac=True,options=dict(maxiter=650,ftol=2e-10,gtol=2e-5,maxls=30))
   q=self.BASE+res.x.reshape(self.n,2); met=self.measure(q)
   print('n',self.n,'seed',seed,'obj',res.fun,'iter',res.nit,'converged',res.success,'metrics',met,flush=True)
   candidates.append((res.fun,q,met))
  good=[c for c in candidates if c[2]['min_sep']>=5.99 and c[2]['blocked']==0]
  return min(good or candidates,key=lambda c:c[0])

if __name__=='__main__':
 import json
 for n in (16,13):
  f=Feast(n);score,q,met=f.solve()
  np.savetxt(OUT/f'feast_n{n}_smoothed_master.csv',q,delimiter=',',header='radial_ft,tangential_ft',comments='')
  (OUT/f'feast_n{n}_smoothed_diagnostics.json').write_text(json.dumps(dict(n=n,objective=score,**met),indent=2))
