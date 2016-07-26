private
entityReaderOn: stream
	"Reading undefined content is allowed for responses"
	
	^ (super entityReaderOn: stream)  
		allowReadingUpToEnd;
		yourself