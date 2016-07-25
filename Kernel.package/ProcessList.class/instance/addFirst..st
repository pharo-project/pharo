adding
addFirst: aLinkOrObject 
	"Add aLink to the beginning of the receiver's list. Answer aLink."
	|aLink|
	aLink := aLinkOrObject asLink.
	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink