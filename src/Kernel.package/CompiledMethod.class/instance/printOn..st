printing
printOn: aStream 
	"Overrides method inherited from the byte arrayed collection."

	aStream print: self methodClass; nextPutAll: '>>'; print: self selector.