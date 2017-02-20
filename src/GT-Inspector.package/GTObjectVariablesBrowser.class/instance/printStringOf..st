private building
printStringOf: anObject 
	^ ["Treat arrays differently as their #printString method can be slow.
		In the future if multiple objects need to be handled differenty,
		dispatch through the object."
		((anObject class == Array  
			ifTrue: [ self printArray: anObject ]
			ifFalse: [ anObject printString ]) contractTo: 200) asComment] 
		on: Error 
		do: ['"Error printing. Try self printString to debug"']