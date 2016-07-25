accessing
fraction
	"Answer the fraction of total that has already been transferred.
	Can be nil. Should be between 0 and 1."
	
	^ self isEmpty ifFalse: [ amount / total ]