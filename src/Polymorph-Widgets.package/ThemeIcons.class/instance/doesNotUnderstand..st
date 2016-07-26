reflective operations
doesNotUnderstand: aMessage
	"WARNING: This is "
	aMessage selector isUnary 
		ifTrue: [ ^ self iconNamed: aMessage selector ].	
	^ super doesNotUnderstand: aMessage