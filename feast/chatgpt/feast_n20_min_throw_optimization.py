"""20-person FEAST with explicit below-6-ft penalty and optional short-pass preference.
Uses the previously defined cyclic master-trace objective and correct exact time reversal.
Run: python feast_n20_min_throw_optimization.py
"""
import os
os.environ.setdefault('OPENBLAS_NUM_THREADS','1')
from pathlib import Path
import numpy as np
from scipy.optimize import minimize
import feast_reflection_optimized_n20 as f

HERE=Path(__file__).resolve().parent
N=f.N
f.B=np.eye(40).reshape(20,2,40)  # unrestricted 40-variable master trace
MIN_PASS=6.0
PREFERRED_MIN=9.0   # soft preference, intentionally distinct from 6-ft hard threshold
LOW_WEIGHT=150.0
SHORT_WEIGHT=2.0


def objective(z):
    val, grad=f.objective(z)
    q=f.make_trace(z)
    beats=f.positions(q,f.C_BEAT)
    delta=beats[f.PAIR_T,f.PAIR_I]-beats[f.PAIR_T,f.PAIR_J]
    d=np.maximum(np.linalg.norm(delta,axis=1),1e-10)
    under6=np.maximum(MIN_PASS-d,0)
    under_preferred=np.maximum(PREFERRED_MIN-d,0)
    val+=LOW_WEIGHT*np.sum(under6**2)+SHORT_WEIGHT*np.sum(under_preferred**2)
    dg=(-2*LOW_WEIGHT*under6-2*SHORT_WEIGHT*under_preferred)/d
    contributions=dg[:,None]*delta
    gb=np.zeros_like(beats)
    np.add.at(gb,(f.PAIR_T,f.PAIR_I),contributions)
    np.add.at(gb,(f.PAIR_T,f.PAIR_J),-contributions)
    gq=f.backprop(gb,f.C_BEAT)
    grad=grad+np.einsum('kdp,kd->p',f.B,gq)
    return val,grad


def pass_stats(q):
    x=f.positions(q,f.C_BEAT)
    d=np.linalg.norm(x[f.PAIR_T,f.PAIR_I]-x[f.PAIR_T,f.PAIR_J],axis=-1)
    # Test full bystander disk clearance of each thrown line at each beat.
    bystander_min=float('inf');blocked=0
    for t,i,j in zip(f.PAIR_T,f.PAIR_I,f.PAIR_J):
        xx=x[t]; v=xx[j]-xx[i]; a=np.clip(((xx-xx[i])@v)/(v@v),0,1)
        perpendicular=np.linalg.norm(xx-(xx[i]+a[:,None]*v),axis=1)
        perpendicular[[i,j]]=np.inf
        md=perpendicular.min()
        bystander_min=min(bystander_min,md)
        blocked+=(md<f.PERSONAL)
    return {'min':float(d.min()),'mean':float(d.mean()),'max':float(d.max()),
            'under6':int(np.sum(d<6-1e-6)),'under9':int(np.sum(d<9-1e-6)),
            'bystander_clearance':bystander_min,'blocked_passes':blocked}


def evaluate(q):
    m=f.measurements(q)
    return {**pass_stats(q),'walk':m['trace_ft'],'separation':m['min_clearance_ft']}


def main():
    current=np.loadtxt(HERE/'feast_unconstrained_n20_trace.csv',delimiter=',',skiprows=1)
    rng=np.random.default_rng(407)
    starts=[current, current+rng.normal(0,.35,current.shape),current+rng.normal(0,1,current.shape)]
    scored=[]
    for k,start in enumerate(starts):
        res=minimize(objective,(start-f.BASE).ravel(),method='L-BFGS-B',jac=True,
              options={'maxiter':650,'ftol':1e-11,'gtol':2e-5,'maxls':35})
        q=f.make_trace(res.x)
        met=evaluate(q)
        print(k,'objective',res.fun,'nit',res.nit,met,flush=True)
        scored.append((res.fun,q,met))
    # Only accept clear straight-line passes and 6-ft personal separations
    valid=[r for r in scored if r[2]['blocked_passes']==0 and r[2]['separation']>=5.99]
    best=min(valid or scored,key=lambda r:r[0])
    print('PREVIOUS',evaluate(current),flush=True)
    print('SELECTED',best[0],best[2],flush=True)
    np.savetxt(HERE/'feast_n20_preferred_long_passes_trace.csv',best[1],delimiter=',',comments='',header='radial_ft,tangential_ft')
    return best

if __name__=='__main__':main()
