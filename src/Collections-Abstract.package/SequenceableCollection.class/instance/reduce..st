enumerating
reduce: aBlock
	"Fold the result of the receiver into aBlock. The argument aBlock must take two or more arguments. It applies the argument, binaryBlock cumulatively to the elements of the receiver. For sequenceable collections the elements will be used in order, for unordered collections the order is unspecified."
	
	"#(1 2 3) reduce: [ :a :b | a + b ]
		--> 1 + 2 + 3 = 6
	#(1 2 3 4 5) reduce: [ :a :b :c | a + b + c ]
		--> 1 + 2 + 3 + 4 + 5 = 15"
		
	^ self reduceLeft: aBlock