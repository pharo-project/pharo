enumerating
do: aBlock without: anItem 
	"Enumerate all elements in the receiver. 
	Execute aBlock for those elements that are not equal to the given item"

	^ self do: [:each | anItem = each ifFalse: [aBlock value: each]]