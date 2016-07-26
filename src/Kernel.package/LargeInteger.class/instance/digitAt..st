system primitives
digitAt: index 
	"Primitive. Answer the value of an indexable field in the receiver.   LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds. Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	self digitLength < index
		ifTrue: [^0]
		ifFalse: [^super at: index]