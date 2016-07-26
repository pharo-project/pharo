inline
caller
	| anObject |
	anObject := 5.
	self 
		called: anObject + 1
		on: [^anObject]