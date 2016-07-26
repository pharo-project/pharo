accessing class hierarchy
addSubclass: aSubclass 
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."
	aSubclass superclass ~~ self 
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		self subclasses: (Array with: aSubclass).
		^ self ].
	self subclasses do:[:cl| cl == aSubclass ifTrue:[^self]]. "Already my subclass"
	self subclasses: (subclasses copyWith: aSubclass).