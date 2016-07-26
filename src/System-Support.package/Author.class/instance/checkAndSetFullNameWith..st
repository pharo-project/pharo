accessing
checkAndSetFullNameWith: aString 
	
	fullName := 
	aString select: [ :character |
		character charCode < 128 and: [ character isAlphaNumeric ]
	].
	
	(fullName = aString and: [ fullName isNotEmpty ]) ifFalse: [ self requestFullName ]
	
	
	