testing method dictionary
includesSelector: aSymbol 
	"Answer whether the message whose selector is the argument is in the 
	method dictionary of the receiver's class."

	^ self methodDict includesKey: aSymbol