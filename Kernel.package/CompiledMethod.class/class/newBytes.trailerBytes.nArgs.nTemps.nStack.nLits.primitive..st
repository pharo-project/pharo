instance creation
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| method pc |
	nArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].

	method := trailer
				createMethod: numberOfBytes
				class: self
				header:    (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue:
		[pc := method initialPC.
		 method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method