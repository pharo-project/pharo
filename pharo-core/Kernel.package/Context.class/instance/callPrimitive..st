instruction decoding
callPrimitive: primNumber
	| res |
	res := receiver tryPrimitive: primNumber withArgs: self arguments.
	primNumber >= (1 << 15) ifTrue: [ "inlined primitive, cannot fail" ^ self push: res ].  
	"regular primitive. Always at the beginning of methods."
	(self isFailToken: res) ifTrue: [ "keeps interpreting the method" ^ self ].
	self push: res.
	self methodReturnTop