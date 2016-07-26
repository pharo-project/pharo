bit manipulation
highBitOfMagnitude
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. This method is used for negative SmallIntegers as well,  
	since Pharo's LargeIntegers are sign/magnitude."
	
	self < 0 ifTrue: [
		"Beware: do not use highBitOfPositiveReceiver
		because self negated is not necessarily a SmallInteger
		(see SmallInteger minVal)"
		^self negated highBitOfMagnitude].
	
	"Implementation note: this method could be as well inlined here."
	^self highBitOfPositiveReceiver