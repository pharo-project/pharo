adding
addLast: aLink 
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty 
		ifTrue: [^firstLink := lastLink := aLink].
	aLink previousLink: lastLink.
	aLink nextLink: nil.
	lastLink ifNotNil: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink