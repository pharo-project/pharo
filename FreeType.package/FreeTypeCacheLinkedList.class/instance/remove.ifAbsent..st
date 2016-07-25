removing
remove: aLink ifAbsent: aBlock  
	| prev next  |

	prev := aLink previousLink.
	next := aLink nextLink.
	prev == nil ifFalse: [prev nextLink: next].
	next == nil ifFalse: [next previousLink: prev].
	aLink == firstLink ifTrue:[firstLink := next].
	aLink == lastLink ifTrue:[lastLink := prev].
	aLink nextLink: nil.
	aLink previousLink: nil.		
	^aLink