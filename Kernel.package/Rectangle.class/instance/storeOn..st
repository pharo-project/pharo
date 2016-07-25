printing
storeOn: aStream 
	"printed form is good for storing too"
	
	aStream nextPut: $(.
	self printOn: aStream.
	aStream nextPut: $).