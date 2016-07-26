class membership
isKindOf: aClass 
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]