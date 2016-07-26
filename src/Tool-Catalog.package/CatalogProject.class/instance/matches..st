testing
matches: aString 
	^ (self matches: self name with: aString)
		or: [ self keywords anySatisfy: [ :each | self matches: each with: aString ] ]