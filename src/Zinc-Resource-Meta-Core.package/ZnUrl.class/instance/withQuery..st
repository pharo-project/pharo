convenience
withQuery: association
	"Return a new URL equal to the receiver with the association's 
	key=value added as a query parameter.
	Note that #queryAt:add: is used."
	
	^ self copy
		queryAt: association key add: association value;
		yourself 
		