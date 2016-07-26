accessing
lastLink
	"Returnt the last link that I hold, also known as my tail.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ tail