comparing
min: aComparable 
	"Answer the receiver or the argument, whichever is lesser."

	self < aComparable
		ifTrue: [^self]
		ifFalse: [^aComparable]