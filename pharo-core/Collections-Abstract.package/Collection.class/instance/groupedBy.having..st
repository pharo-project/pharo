enumerating
groupedBy: aBlock having: aSelectionBlock
	"Like in SQL operation - Split the recievers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock."
	^ (self groupedBy: aBlock) select: aSelectionBlock
