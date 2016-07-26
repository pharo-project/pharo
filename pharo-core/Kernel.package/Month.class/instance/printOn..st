printing
printOn: aStream 

	aStream nextPutAll: self monthName, ' ', self year printString.