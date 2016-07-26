accessing
nextPage: theIndexOfAnElement
	"To make sure we can go to the page of an element."

	self currentPage: (self pageOfNodeIndex: theIndexOfAnElement)