private building
printArray: anArray
	"The #printString method of an array needs to scan through the entire array to
	determine how to print the array. Only scan through a limited number of elements
	for large arrays."
	| limitedString limit |
	limit := 50000.
	limitedString := String streamContents: [ :aStream | 
		anArray gtInspectorFastPrintOn: aStream.
	] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '...etc...'
	
	