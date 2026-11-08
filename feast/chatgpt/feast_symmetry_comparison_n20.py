"""Controlled n=20 FEAST symmetry test using exact objective from feast_reflection_optimized_n20.py.
Optimizes a shared master trace with 20 or 40 free variables and verifies reflection invariance.
"""
import os
os.environ.setdefault('OPENBLAS_NUM_THREADS','1')
import numpy as np
from scipy.optimize import minimize
import feast_reflection_optimized_n20 as f
from pathlib import Path
import csv

OUT=Path(__file__).resolve().parent
q0=np.loadtxt(OUT/'feast_reflection_optimized_n20_trace.csv',delimiter=',',skiprows=1)
Bs=f.B.copy()
Bu=np.eye(40).reshape(20,2,40)

def calc(q, symmetric=False):
    f.B=Bs if symmetric else Bu
    z=np.linalg.lstsq(Bs.reshape(40,20),np.reshape(q-f.BASE,(40,)),rcond=None)[0] if symmetric else (q-f.BASE).ravel()
    return f.objective(z)[0]

def mirror(q):
    ret=q[(-np.arange(f.N))%f.N].copy()
    ret[:,1]*=-1
    return ret

def opt(z, B, maxiter=550):
    f.B=B
    res=minimize(f.objective,z,method='L-BFGS-B',jac=True,
      options={'maxiter':maxiter,'ftol':1e-11,'gtol':5e-5,'maxls':30})
    q=f.make_trace(res.x)
    return res,q

# Reoptimize the original symmetry-constrained trace to remove solver tolerance effects.
z0=np.linalg.lstsq(Bs.reshape(40,20),(q0-f.BASE).reshape(-1),rcond=None)[0]
rs,qs=opt(z0,Bs,650)
print('symmetric objective',rs.fun,'metrics',f.measurements(qs),flush=True)

# Unconstrained starts include best symmetric solution, small perturbations,
# and large asymmetry to test for symmetry breaking.
rng=np.random.default_rng(2705)
starts=[qs,qs+rng.normal(0,.15,(20,2)),qs+rng.normal(0,1.1,(20,2)),
        qs+rng.normal(0,3.0,(20,2)), mirror(qs)+rng.normal(0,1.5,(20,2))]
results=[]
for a,start in enumerate(starts):
    ru,qu=opt((start-f.BASE).ravel(),Bu,650)
    obj=calc(qu)
    reflect=calc(mirror(qu))
    m=f.measurements(qu)
    deviation=float(np.sqrt(np.mean((qu-mirror(qu))**2)))
    print('unconstrained',a,'objective',obj,'mirror objective',reflect,'RMS asym',deviation,'metrics',m,'success',ru.success,flush=True)
    results.append((obj,qu,m,deviation))

best=min(results,key=lambda a:a[0])
np.savetxt(OUT/'feast_unconstrained_n20_trace.csv',best[1],delimiter=',',header='radial_ft,tangential_ft',comments='')
np.savetxt(OUT/'feast_symmetric_control_n20_trace.csv',qs,delimiter=',',header='radial_ft,tangential_ft',comments='')
with open(OUT/'feast_symmetry_comparison_n20_results.csv','w',newline='') as ff:
  wr=csv.writer(ff);wr.writerow(['run','objective','walk_ft','longest_throw_ft','minimum_spacing_ft','mirror_asymmetry_rms_ft'])
  ms=f.measurements(qs)
  wr.writerow(['symmetric',rs.fun,ms['trace_ft'],ms['max_throw_ft'],ms['min_clearance_ft'],0])
  for k,(obj,q,m,d) in enumerate(results):wr.writerow([f'unconstrained_{k}',obj,m['trace_ft'],m['max_throw_ft'],m['min_clearance_ft'],d])
print('DONE best',best[0],'vs symmetric',rs.fun,flush=True)
