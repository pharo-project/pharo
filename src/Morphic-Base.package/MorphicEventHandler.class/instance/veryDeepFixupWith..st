copying
veryDeepFixupWith: deepCopier
	| old |
	"ALL inst vars were weakly copied.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

	super veryDeepFixupWith: deepCopier.
	1 to: self class instSize do: [:ii | 
		old := self instVarAt: ii.
		self instVarAt: ii put: (deepCopier references at: old ifAbsent: [old])
		].

