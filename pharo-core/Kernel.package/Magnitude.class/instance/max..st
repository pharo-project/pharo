comparing
max: aMagnitude 
	"Answer the receiver or the argument, whichever has the greater 
	magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]