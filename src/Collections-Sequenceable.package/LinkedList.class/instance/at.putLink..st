accessing
at: index putLink: aLink 
	| previousLink nextLink |
	"Please don't put a link which is already in the list, or you will create an infinite loop"
	(self validIndex: index)
		ifFalse: [^ self errorOutOfBounds].
	index = 1
		ifTrue: [aLink nextLink: self firstLink nextLink.
			firstLink := aLink.
			aLink nextLink ifNil: [lastLink := aLink].
			^ aLink].
	previousLink := self linkAt: index - 1.
	nextLink := previousLink nextLink nextLink.
	
	nextLink ifNil: [
		aLink nextLink: self lastLink
	] ifNotNil: [
		aLink nextLink: nextLink.
	].

	previousLink nextLink: aLink.

	nextLink ifNil: [
		lastLink := aLink.
		aLink nextLink: nil.
	].

	^ aLink