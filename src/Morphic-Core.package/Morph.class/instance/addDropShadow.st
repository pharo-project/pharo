drop shadows
addDropShadow

	self hasDropShadow ifTrue:[^self].
	self changed.
	self hasDropShadow: true.
	self shadowOffset: 3@3.
	self layoutChanged.
	self changed.