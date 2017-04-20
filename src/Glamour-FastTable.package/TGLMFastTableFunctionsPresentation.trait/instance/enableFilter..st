public api
enableFilter: aBlock
	"aBlock receives two parameters: 
		- an item
		- a pattern.
	It should return true if the item should be displayed; false otherwise."
	self enableFilter.
	self parameters at: 'filterBlock' put: aBlock
	