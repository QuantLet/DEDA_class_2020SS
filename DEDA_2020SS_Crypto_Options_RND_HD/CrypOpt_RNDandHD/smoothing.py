import numpy as np
from scipy.stats import norm
import scipy.interpolate as interpolate  # B-Spline


def _gaussian_kernel(M, m, h_m, T, t, h_t):
    u_m = (M-m)/h_m
    u_t = (T-t)/h_t
    return norm.cdf(u_m) * norm.cdf(u_t)


def _epanechnikov(M, m, h_m, T, t, h_t):
    u_m = (M-m)/h_m
    u_t = (T-t)/h_t
    return 3/4 * (1-u_m)**2 * 3/4 * (1-u_t)**2


def _local_polynomial(df, m, t, h_m, h_t, kernel=_gaussian_kernel):
    M = np.array(df.M)
    T = np.array(df.tau)
    y = np.array(df.iv)
    n = df.shape[0]

    X1 = np.ones(n)
    X2 = M - m
    X3 = (M-m)**2
    X4 = T-t
    X5 = (T-t)**2
    X6 = X2*X4
    X = np.array([X1, X2, X3, X4, X5, X6]).T

    ker = kernel(M, m, h_m, T, t, h_t)
    W = np.diag(ker)

    XTW = np.dot(X.T, W)

    beta = np.linalg.pinv(np.dot(XTW, X)).dot(XTW).dot(y)

    return beta[0], beta[1], 2*beta[2]


def locpoly_smoothing(df, tau, h_m, h_t=0.05, gridsize=50, kernel='epak'):

    if kernel=='epak':
        kernel = _epanechnikov
    elif kernel=='gauss':
        kernel = _gaussian_kernel
    else:
        print('kernel not know, use epanechnikov')
        kernel = _epanechnikov

    num = gridsize
    M_min, M_max = min(df.M), max(df.M)
    M = np.linspace(M_min, M_max, gridsize)

    sig = np.zeros((num, 3))
    for i, m in enumerate(M):
        sig[i] = _local_polynomial(df, m, tau, h_m, h_t, kernel)

    smile = sig[:, 0]
    first = sig[:, 1]
    second = sig[:, 2]

    S_min, S_max = min(df.S), max(df.S)
    K_min, K_max = min(df.K), max(df.K)
    S = np.linspace(S_min, S_max, gridsize)
    K = np.linspace(K_min, K_max, gridsize)

    return smile, first, second, M, S, K


def bspline(M, smile, sections, degree=3):
    idx = np.linspace(0, len(M) - 1, sections+1, endpoint=True).round(0).astype('int')
    x = M[idx]
    y = smile[idx]

    t, c, k = interpolate.splrep(x, y, s=0, k=degree)
    spline = interpolate.BSpline(t, c, k, extrapolate=True)
    pars = {'t': t, 'c': c, 'deg': k}
    points = {'x': x, 'y': y}
    return pars, spline, points