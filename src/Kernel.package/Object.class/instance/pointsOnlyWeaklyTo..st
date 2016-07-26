pointing to
pointsOnlyWeaklyTo: anObject
	"Assume, we already know that receiver points to an object,
	answer true if receiver points only weakly to it "
	
	self class isWeak ifFalse: [ ^ false ].
	1 to: self class instSize do:
		[:i | (self instVarAt: i) == anObject ifTrue: [^ false]].
	^ true