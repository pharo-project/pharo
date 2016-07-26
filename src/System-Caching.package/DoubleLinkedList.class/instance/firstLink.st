accessing
firstLink
	"Returnt the first link that I hold, also known as my head.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ head