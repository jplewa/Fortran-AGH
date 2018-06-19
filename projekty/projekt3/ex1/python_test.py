import mat_ops
import numpy

print("Gauss-Jordan elimination of")
A = [1,2,1,1,3,3,1,7,-2]
X = [3,0,17]
print(numpy.transpose(numpy.array(A).reshape(3,3)))
print(X)

print("Result")
(B, Y, _) = mat_ops.mat_ops.gauss(numpy.array(A).reshape(3,3),numpy.array(X).reshape(3), 3)
print(numpy.transpose(numpy.array(B).reshape(3,3)))
print(Y)

print("\nMatrix multiplication of")
M1 = [1,4,7,2,5,8,3,6,9]
M2 = [9,6,3,8,5,2,7,4,1]
print(numpy.transpose(numpy.array(M1).reshape(3,3)))
print(numpy.transpose(numpy.array(M2).reshape(3,3)))

print("Result")
R = mat_ops.mat_ops.mm(numpy.transpose(numpy.array(M1).reshape(3,3)),numpy.transpose(numpy.array(M2).reshape(3,3)), 3)
print(numpy.array(R).reshape(3,3))