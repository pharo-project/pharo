accessing class hierarchy
superclass: aClass 
	"Change the receiver's superclass to be aClass."
	"Note: Do not use 'aClass isKindOf: Behavior' here
		in case we recompile from Behavior itself."
	(aClass isNil or: [aClass isBehavior])
		ifTrue: [superclass := aClass.
				Object flushCache]
		ifFalse: [self error: 'superclass must be a class-describing object']