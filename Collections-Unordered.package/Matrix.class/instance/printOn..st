printing
printOn: aStream

	aStream nextPutAll: '('.
	(1 to: self numberOfRows) 
		do: [ :r | 
				(self atRow: r) 
					do: [ :each | aStream nextPutAll: each printString ] 
					separatedBy: [ aStream space ]]
		separatedBy: [ aStream cr ].
	aStream nextPutAll: ' )'.