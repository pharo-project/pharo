private
carefullyPrint: anObject on: aStream
	aStream nextPutAll: ([anObject printString]
		on: Error
		do: ['unprintable ' , anObject class name])