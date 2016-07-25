enumerating
do: aBlock without: anItem
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]