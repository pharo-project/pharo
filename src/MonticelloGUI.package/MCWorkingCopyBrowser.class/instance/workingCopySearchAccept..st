morphic ui
workingCopySearchAccept: string
	| aString |

	aString := string ifNil: [ '' ].
	workingCopyPattern = aString asLowercase ifTrue: [ ^ self ].
	workingCopyPattern := aString asLowercase.
		
	packageProcess ifNotNil: [ packageProcess terminate ].	
	packageProcess := [ self workingCopyListChanged ] fork.