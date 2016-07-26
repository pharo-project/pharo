nonhomogeneous accessing
nextNumber: n put: v 
	"Append to the receiver the argument, v, which is a positive 
	SmallInteger or a LargePositiveInteger, as the next n bytes.
	Possibly pad with leading zeros."

	1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
	^ v
