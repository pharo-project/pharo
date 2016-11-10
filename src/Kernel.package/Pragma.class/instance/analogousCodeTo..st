comparing
analogousCodeTo: anObject 
	^self class == anObject class
	  and: [selector == anObject keyword
	  and: [arguments = anObject arguments]]