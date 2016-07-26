comparing
< aMagnitude 
	"Answer whether the receiver is less than the argument."

	(self size = aMagnitude size) ifFalse: [ 
		^ self size < aMagnitude size ].
	1 to: self size do: [:i |
		(self at: i) = (aMagnitude at: i) ifFalse: [
			^ (self at: i) < (aMagnitude at: i) ]].			
	^false.