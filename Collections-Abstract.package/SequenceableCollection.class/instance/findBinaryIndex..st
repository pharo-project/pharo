enumerating
findBinaryIndex: aBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]
	"
	^ self 
		findBinaryIndex: aBlock 
		do: [ :found | found ] 
		ifNone: [ self errorNotFound: aBlock ]