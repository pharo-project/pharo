private building
printStringOf: anObject
	^ [(anObject printString contractTo: 200) asComment] 
			on: Error 
			do: ['"Error printing. Try self printString to debug"']