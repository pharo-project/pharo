instance creation
hostObject: aContext tempName: aString tempIndex: anInteger
	^ (self hostObject: aContext) 
		tempName: aString;
		tempIndex: anInteger;
		variableTag: ((aContext method argumentNames includes: aString)
			ifTrue: [ 'parameter' ]
			ifFalse: [ 'temp' ])
		yourself