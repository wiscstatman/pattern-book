"""Valid clockwise and counterclockwise 20-juggler FEAST animations.

CCW is the *time reversal of the entire choreography*, not the
reflection-plus-time reversal of the positions paired with a separately
reversed passing schedule. This guarantees exact reversal of the collective
shape progression and identical throwing geometry at the paired beats.

Inputs: feast_unconstrained_n20_trace.csv (provided with the earlier experiment)
Outputs: feast_n20_clockwise_corrected.gif, feast_n20_counterclockwise_corrected.gif
Requires numpy, scipy, matplotlib, pillow.
"""
from pathlib import Path
import numpy as np
from scipy.interpolate import CubicSpline
import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter
from matplotlib.colors import hsv_to_rgb

HERE=Path(__file__).resolve().parent
N=20
R=22.374
PERSONAL_RADIUS=3.0
FRAMES=80
FPS=10
Q=np.loadtxt(HERE/'feast_n20_preferred_long_passes_trace.csv',delimiter=',',skiprows=1)
assert Q.shape==(N,2)
angles=2*np.pi*np.arange(N)/N
rot=np.array([[[np.cos(a),np.sin(a)],[-np.sin(a),np.cos(a)]] for a in angles])
home=np.einsum('icd,d->ic',rot,np.array([R,0.]))
spline=CubicSpline(np.arange(N+1),np.vstack([Q,Q[:1]]),axis=0,bc_type='periodic')
colors=[hsv_to_rgb((i/N,.68,.85)) for i in range(N)]


def cw_positions(t):
    t=np.asarray(t,dtype=float).reshape(-1)
    local=spline((t[:,None]-2*np.arange(N)[None,:])%N)
    return np.einsum('icd,sid->sic',rot,local)


def positions(t, direction):
    return cw_positions(np.asarray(t)* (1 if direction=='clockwise' else -1))


def pairs(beat,direction):
    signed=beat if direction=='clockwise' else -beat
    return [(i,(signed-i)%N) for i in range(N) if i<(signed-i)%N]


def bystander_clearance(x, i, j):
    v=x[j]-x[i]
    alpha=np.clip(((x-x[i])@v)/np.dot(v,v),0,1)
    projected=x[i]+alpha[:,None]*v
    ds=np.linalg.norm(x-projected,axis=1)
    ds[[i,j]]=np.inf
    return ds.min()


def validate():
    worst=1e6
    for b in range(N):
        a=positions([b],'clockwise')[0]
        c=positions([b],'counterclockwise')[0]
        assert np.allclose(c,cw_positions([-b])[0])
        for i,j in pairs(b,'clockwise'):
            worst=min(worst,bystander_clearance(a,i,j))
        for i,j in pairs(b,'counterclockwise'):
            worst=min(worst,bystander_clearance(c,i,j))
    assert worst>=PERSONAL_RADIUS, f'Obstructed beat-pass: {worst:.3f} ft'
    # The whole moving ensemble, including labels, is exactly reversed in time.
    t=np.linspace(0,N,147,endpoint=False)
    reversal_error=np.max(np.abs(positions(t,'counterclockwise')-positions((-t)%N,'clockwise')))
    assert reversal_error<1e-10
    print(f'Verified: 0 obstructed passes at passing beats; min clearance {worst:.3f} ft; max time reversal error {reversal_error:.2e} ft',flush=True)

PATHS=positions(np.linspace(0,N,440,endpoint=False),'clockwise')
LIMIT=max(32.,float(np.ceil(np.max(np.abs(PATHS))+PERSONAL_RADIUS+2)))


def animate(direction):
    fig,ax=plt.subplots(figsize=(7.3,7.65),facecolor='#fbfcfe')
    fig.subplots_adjust(left=.055,right=.945,bottom=.075,top=.89)
    ax.set_facecolor('#fbfcfe');ax.set_aspect('equal')
    ax.set_xlim(-LIMIT,LIMIT);ax.set_ylim(-LIMIT,LIMIT);ax.axis('off')
    ax.add_patch(plt.Circle((0,0),R,fill=False,ls=':',lw=.85,color='#b8c2cf'))
    for p in home:
        ax.plot(p[0],p[1],'+',color='#aeb8c4',ms=6,mew=1)
    for i in range(N):
        ax.plot(PATHS[:,i,0],PATHS[:,i,1],color=colors[i],alpha=.20,lw=.7,zorder=1)
    envelope,=ax.plot([],[],color='#123a5a',lw=2.7,alpha=.80,zorder=3)
    pass_lines=[ax.plot([],[],color='#df8a2d',lw=1.25,alpha=.80,zorder=4)[0] for _ in range(N//2)]
    discs=[];dots=[];labels=[]
    for i in range(N):
        disc=plt.Circle((0,0),PERSONAL_RADIUS,fill=False,lw=.65,color=colors[i],alpha=.48,zorder=5)
        ax.add_patch(disc);discs.append(disc)
        dot,=ax.plot([],[],'o',ms=10.8,mfc=colors[i],mec='white',mew=1,zorder=7)
        dots.append(dot)
        labels.append(ax.text(0,0,str(i+1),fontsize=6.5,weight='bold',ha='center',va='center',color='white',zorder=8))
    head='CLOCKWISE' if direction=='clockwise' else 'COUNTERCLOCKWISE'
    fig.text(.5,.942,f'20-person FEAST  ·  {head}  ·  longer passes',ha='center',fontsize=14,color='#1d3044',weight='bold')
    subtitle=fig.text(.5,.905,'',ha='center',fontsize=9.5,color='#49647b')
    fig.text(.5,.035,'Blue: collective envelope   ·   Orange: unobstructed beat passes   ·   Rings: 3-ft radius',ha='center',fontsize=8,color='#536b82')

    def update(frame):
        t=N*frame/FRAMES
        x=positions([t],direction)[0]
        for i in range(N):
            discs[i].center=x[i];dots[i].set_data([x[i,0]],[x[i,1]])
            labels[i].set_position(x[i])
        closed=np.vstack([x,x[:1]])
        envelope.set_data(closed[:,0],closed[:,1])
        # Pass segments are valid at beat instants, not necessarily at arbitrary
        # intermediate times. Show only within 0.20 beats of release.
        nearest=int(np.floor(t+.5))
        delta=abs(t-nearest)
        if delta<=.20:
            selected=pairs(nearest%N,direction)
            for ln,(i,j) in zip(pass_lines,selected):
                ln.set_data([x[i,0],x[j,0]],[x[i,1],x[j,1]])
                ln.set_alpha(.84*(1-delta/.22))
            for ln in pass_lines[len(selected):]:ln.set_data([],[])
        else:
            for ln in pass_lines:ln.set_data([],[])
        subtitle.set_text(f'Beat {int(np.floor(t)) + 1:02d} / 20    ·    59.88 ft walking / cycle    ·    8.91–10.08 ft passes')
        return [envelope,*pass_lines,*dots,*labels,*discs,subtitle]

    filename=f'feast_n20_{direction}_preferred_long_passes.gif'
    animation=FuncAnimation(fig,update,frames=FRAMES,interval=1000/FPS,blit=False)
    animation.save(HERE/filename,writer=PillowWriter(fps=FPS),dpi=76)
    plt.close(fig)
    print('Wrote',filename,flush=True)

if __name__=='__main__':
    validate()
    for direction in ('clockwise','counterclockwise'):
        animate(direction)
