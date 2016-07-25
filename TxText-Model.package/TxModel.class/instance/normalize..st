private
normalize: selection
	"Merge adjacent spans if they have the same style"

	| span normalizedSelection |
	normalizedSelection := selection normalized.
	span := normalizedSelection start span prev.
	
	[ span tryMergeWithNextPreserving: selection.
	  span == normalizedSelection end span ] whileFalse: [ span := span next ].

