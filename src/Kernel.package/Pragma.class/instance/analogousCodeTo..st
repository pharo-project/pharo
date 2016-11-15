comparing
analogousCodeTo: anObject 
	^self class == anObject class
	  and: [keyword == anObject keyword
	  and: [arguments = anObject arguments]]