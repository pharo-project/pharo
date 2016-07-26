generating C program
writeCInclude: aHeaderName on: aStream
	"Internal method for the C program creation. Giving aHeaderName like 'signal.h' this method writes the C #include definition."
	aStream 
		nextPutAll: '#include <';
		nextPutAll: aHeaderName;
		nextPutAll: '>';
		cr.	
