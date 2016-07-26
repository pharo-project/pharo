comparing
max: aComparable 
	"Answer the receiver or the argument, whichever is greater."

	self > aComparable
		ifTrue: [^self]
		ifFalse: [^aComparable]