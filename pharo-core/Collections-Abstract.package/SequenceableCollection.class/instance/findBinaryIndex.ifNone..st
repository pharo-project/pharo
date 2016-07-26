enumerating
findBinaryIndex: aBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements as optional arguments."

	^ self 
		findBinaryIndex: aBlock 
		do: [ :found | found ] 
		ifNone: exceptionBlock