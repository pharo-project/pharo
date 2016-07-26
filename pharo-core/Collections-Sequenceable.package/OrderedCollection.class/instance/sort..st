enumerating
sort: aSortBlock 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	array
		mergeSortFrom: firstIndex
		to: lastIndex 
		src: array shallowCopy 
		dst: array 
		by: aSortBlock