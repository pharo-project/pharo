testing
atEnd
	"I am atEnd when there is no more data to be read and there never will be.
	This means that readBuffer must be empty, 
	there must be no more unread data available at the socket,
	and the socket must be closed"
	
	^ self subclassResponsibility 