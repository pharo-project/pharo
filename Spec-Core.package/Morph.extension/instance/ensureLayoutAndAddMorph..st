*Spec-Core
ensureLayoutAndAddMorph: aMorph

	aMorph layoutFrame
		ifNil: [ aMorph layoutFrame: LayoutFrame identity].
	self addMorph: aMorph 