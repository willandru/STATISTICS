# LA MEDIA


# FUNCTION

def mi_media(lista):
	return float(sum(lista))/len(lista)

x_input= [1, 3, 4, 5, 5, 4, 5, 5, 4, 3]

print (mi_media(x_input))



# USING NUMPY

import numpy
print( numpy.mean(x_input))