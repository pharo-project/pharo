accessing
comment: aString
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	comment := aString isRemoteString
		ifTrue: [ aString ]
		ifFalse: [ 
			aString isEmptyOrNil
				ifTrue: [ nil ]
				ifFalse: [ SourceFiles remoteStringForNewString: aString ] ]