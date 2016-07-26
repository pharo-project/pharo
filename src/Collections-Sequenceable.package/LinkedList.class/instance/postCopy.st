copying
postCopy
	| aLink |
	super postCopy.
	firstLink ifNotNil: [
		aLink := firstLink := firstLink copy.
		[aLink nextLink isNil] whileFalse: [aLink nextLink: (aLink := aLink nextLink copy)].
		lastLink := aLink].