comparing
min: aMagnitude 
	"Answer the receiver or the argument, whichever has the lesser 
	magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]