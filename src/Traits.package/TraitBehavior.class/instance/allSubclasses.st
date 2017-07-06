accessing class hierarchy
allSubclasses
	"Answer an orderedCollection of the receiver's and the receiver's descendent's subclasses. "

	| scan scanTop |
	scan := OrderedCollection withAll: self subclasses.
	scanTop := 1.
	[scanTop > scan size]
		whileFalse: [scan addAll: (scan at: scanTop) subclasses.
			scanTop := scanTop + 1].
	^ scan