initialize
irPrimitive: anIrPrimitive
	literals isEmpty ifFalse: [self error: 'init prim before adding instructions'].
	anIrPrimitive spec ifNotNil: [literals add: anIrPrimitive spec].
	primNumber := anIrPrimitive num.
	primNumber = 0 ifTrue: [ ^ self ].
	self encoderClass callPrimitiveCode ifNil: [ ^ self ].
	encoder genCallPrimitive: primNumber