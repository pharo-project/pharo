updating
addPragma: aPragma
	"if aPragma is to be kept, then add it and announce its adding"
	(self keepPragma: aPragma)
		ifTrue: [self collected add: aPragma.
			self announce: (PragmaAdded pragma: aPragma)]