printing
printOn: aStream 
	"Overrides method inherited from the byte arrayed collection."

	aStream 
		print: class; 
		nextPutAll: '>>'; 
		store: self selector; 
		nextPutAll: ' (ReflectiveMethod)'.