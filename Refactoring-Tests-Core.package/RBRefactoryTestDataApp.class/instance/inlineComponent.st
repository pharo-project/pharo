inline
inlineComponent
	| a |
	a := 5.
	^a class
		superclass;
		hasImmediateInstances;
		yourself