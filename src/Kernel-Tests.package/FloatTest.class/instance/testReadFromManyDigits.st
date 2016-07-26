tests - conversion
testReadFromManyDigits
	"A naive algorithm may interpret these representations as Infinity or NaN.
	This is http://bugs.squeak.org/view.php?id=6982"
	
	| s1 s2 |
	s1 := '1' , (String new: 321 withAll: $0) , '.0e-321'.
	s2 := '0.' , (String new: 320 withAll: $0) , '1e321'.
	self assert: (Number readFrom: s1) = 1.
	self assert: (Number readFrom: s2) = 1.