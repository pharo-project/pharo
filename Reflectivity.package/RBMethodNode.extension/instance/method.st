*Reflectivity
method
	"return the method that I have been compiled for"
	self compilationContext ifNil: [ ^nil ].
	^self compilationContext getClass >> self selector