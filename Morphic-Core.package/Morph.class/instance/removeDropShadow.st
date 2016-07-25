drop shadows
removeDropShadow
	self hasDropShadow ifFalse:[^self].
	self changed.
	self hasDropShadow: false.
	self shadowOffset: 0@0.
	self layoutChanged.
	self changed.