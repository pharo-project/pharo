operations
<= anEntry
	^ date = anEntry date 
		ifTrue: [contents <= anEntry contents]
		ifFalse: [date > anEntry date]