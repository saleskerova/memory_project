import numpy as np
from scipy.io.wavfile import write
from matplotlib import pyplot as plt

#A = 523.25  # Hz
fq = 523.25
samplerate = 44100
t = np.linspace(0, 1, samplerate)

#bgl = [1, 18/17, 12/11, 9/8, 81/68, 4/3, 24/17, 16/11, 3/2, 27/17, 18/11, 27/16, 16/9, 32/17, 64/33, 2]
data = np.array([])


data = np.append(data, np.sin(2. * np.pi * fq * t) + (0.5 *np.sin(2. * np.pi * (2*fq) * t)) + (0.25*(np.sin(2. * np.pi * (4*fq) * t)))



fs = 100 # sample rate
f = 2 # the frequency of the signal

x = np.arange(fs) # the points on the x axis for plotting
# compute the value (amplitude) of the sin wave at the for each sample
y = np.sin(2*np.pi*f * (x/fs))

#this instruction can only be used with IPython Notbook.
% matplotlib inline
# showing the exact location of the smaples
plt.stem(x,y, 'r', )
plt.plot(x,y)




#write("c5.wav", samplerate, data)



#fq = A * bgl[i]
#np.sin(2. * np.pi * (2*fq) * t) + np.sin(2. * np.pi * (4*fq) * t)
