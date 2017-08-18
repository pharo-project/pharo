stdio
voidStdioFiles
		
	Smalltalk os isWin32 ifTrue: [ 
		{Stdout . Stderr } do: [ :each | 
			[ 	each ifNotNil: [ 
					each size isZero ifTrue: [ 
						each close.
						each name asFileReference delete	 
					] 
				]
			] ifError: [ ].
		]
	].
	
	Stdin := Stdout := Stderr := StdioFiles := nil