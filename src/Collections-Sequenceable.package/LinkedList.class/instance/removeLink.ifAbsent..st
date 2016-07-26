removing
removeLink: aLink ifAbsent: aBlock  
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink := aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := nil]]
		ifFalse: [tempLink := firstLink.
				[tempLink == nil ifTrue: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink := tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := tempLink]].
	"Not nilling the link enables us to delete while iterating"
	"aLink nextLink: nil."
	^aLink