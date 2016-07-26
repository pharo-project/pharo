adding
addFirst: aLink 
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	self isEmpty 
		ifTrue: [^lastLink :=firstLink := aLink].
	aLink nextLink: firstLink.
	aLink previousLink: nil.
	firstLink ifNotNil: [firstLink previousLink: aLink].
	firstLink := aLink.
	^aLink