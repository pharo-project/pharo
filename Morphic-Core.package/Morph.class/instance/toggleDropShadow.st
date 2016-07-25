drop shadows
toggleDropShadow
	self hasDropShadow
		ifTrue:[self removeDropShadow]
		ifFalse:[self addDropShadow].