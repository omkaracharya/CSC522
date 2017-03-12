import numpy as np
import math
import matplotlib.pyplot as plt


def histogram(img):
    img = img.flatten('F')
    tam = len(img)
    m = math.ceil(max(img)) + 1
    h = np.zeros(m)
    for i in range(0, tam):
        f = math.floor(img[i])
        if (f > 0 or f < (m - 1)):
            a2 = img[i] - f
            a1 = 1 - a2
            h[f - 1] = h[f - 1] + a1
            h[f] = h[f] + a2
    h = np.convolve(h, [1, 2, 3, 2, 1])
    h = h[3:(len(h) - 1)]
    h /= sum(h)
    return h


def distribution(mu, v, p, x):
    mu = mu.flatten('F')
    v = v.flatten('F')
    p = p.flatten('F')
    x = x.flatten('F')
    y = np.zeros((x.shape[0], mu.shape[0]))
    for i in range(mu.shape[0]):
        d = x - mu[i]
        amp = p[i] / np.sqrt(2 * math.pi * v[i])
        y[:, i] = amp * np.exp(-0.5 * (d * d) / v[i])
    return y


def main():
    img = plt.imread('../images/ManUtd.jpg')
    copy = img
    img = img.flatten('F')
    minValue = min(img)
    img = img - minValue + 1
    maxValue = max(img)

    h = histogram(img)
    x = np.where(h != 0)[0]
    h = h[np.nonzero(h)]
    h = h.flatten('F')
    x = x.flatten('F')

    k = 4
    mu = np.multiply(list(range(1, k + 1)), (maxValue / (k + 1)))
    v = np.ones(k) * maxValue
    p = np.ones(k) / k

    sml = np.mean(np.diff(x)) / 1000

    #     EM Algorithm

    while (1):
        prb = distribution(mu, v, p, x)
        scal = np.sum(prb, axis=1)
        loglik = sum(h * np.log(scal))

        for j in range(k):
            pp = h * prb[:, j] / scal
            p[j] = sum(pp)
            mu[j] = sum(x * pp) / p[j]
            vr = x - mu[j]
            v[j] = sum(vr * vr * pp) / p[j] + sml

        p = p + 1e-3
        p = p / sum(p)

        prb = distribution(mu, v, p, x)
        scal = np.sum(prb, axis=1) + np.finfo(np.float).eps
        nloglik = sum(h * np.log(scal))
        if (nloglik - loglik) < 1e-3:
            break

    print(mu, v, p)
    plt.clf()
    plt.plot(x, h, 'b-')
    plt.hold(True)
    plt.plot(x, prb, 'r--')
    plt.plot(x, np.sum(prb, axis=1), 'g-')
    plt.show()

    # mu = mu + minValue - 1
    # s = copy.shape
    # mask = np.zeros(s)
    # c = np.zeros(k)
    # for i in range(s[0]):
    #     for j in range(s[1]):
    #         for n in range(k):
    #             c[n] = distribution(mu[n], v[n], p[n], copy[i][j][0])
    #         a = np.where(c == max(c))
    #         mask[i][j] = a[0][0]


if __name__ == '__main__':
    main()