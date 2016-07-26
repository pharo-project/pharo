accessing
schemeOrDefault
	^ self hasScheme 
		ifTrue: [ self scheme ]
		ifFalse: [ #http ]    