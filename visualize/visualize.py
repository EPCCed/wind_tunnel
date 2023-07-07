import numpy as np
import matplotlib.pylab as plt


def visualize(filename):

    f= open(outputfile, "rb" )

    nx=np.fromfile(f,count=1,dtype=np.int32)[0]
    ny=np.fromfile(f,count=1,dtype=np.int32)[0]

    x=np.fromfile(f,count=nx,dtype=np.float64)
    x=np.linspace(np.min(x),np.max(x),num=(len(x)) )
    y=np.fromfile(f,count=ny,dtype=np.float64)
    y=np.linspace(np.min(y),np.max(y),num=(len(y)) )

    psi=np.fromfile(f,count=nx*ny,dtype=np.float64).reshape((nx,ny))
    mask=np.fromfile(f,count=nx*ny,dtype=np.int32).reshape((nx,ny))
    u=np.fromfile(f,count=nx*ny,dtype=np.float64).reshape((nx,ny))
    v=np.fromfile(f,count=nx*ny,dtype=np.float64).reshape((nx,ny))
    u2=np.fromfile(f,count=nx*ny,dtype=np.float32).reshape((nx,ny))
    p=np.fromfile(f,count=nx*ny,dtype=np.float32).reshape((nx,ny))

    plt.streamplot(x,y,u,v,density=1,color="green",arrowstyle='fancy')
    plt.imshow(u2,origin='lower',extent=(x[0],x[-1],y[0],y[-1]))

    plt.xlabel("x")
    plt.ylabel("y")
