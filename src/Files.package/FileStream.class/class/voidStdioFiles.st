stdio
voidStdioFiles
		
	Smalltalk os isWin32 ifTrue: [ 
		{Stdout . Stderr } 
			select: [ :each | each isNotNil and: [ each size isZero ] ] 
			thenDo: [ :each | 
				[	| fref | 
					fref := each name asFileReference.
					each close.
					fref delete
				] ifError: [ ]]
	].
	
	Stdin := Stdout := Stderr := StdioFiles := nil