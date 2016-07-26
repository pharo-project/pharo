testing
includesAll: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true