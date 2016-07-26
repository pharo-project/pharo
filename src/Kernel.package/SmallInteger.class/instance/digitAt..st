system primitives
digitAt: n 
	"Answer the value of an indexable field in the receiver.  LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds."
	n>4 ifTrue: [^ 0].
	self < 0
		ifTrue: 
			[self = SmallInteger minVal ifTrue:
				["Can't negate minVal -- treat specially"
				^ #(0 0 0 64) at: n].
			^ ((0-self) bitShift: (1-n)*8) bitAnd: 16rFF]
		ifFalse: [^ (self bitShift: (1-n)*8) bitAnd: 16rFF]