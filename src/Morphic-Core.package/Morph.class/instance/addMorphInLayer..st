wiw support
addMorphInLayer: aMorph

	submorphs do: [ :each |
		each == aMorph ifTrue: [^self].
		aMorph morphicLayerNumber < each morphicLayerNumber ifTrue: [
			^self addMorph: aMorph inFrontOf: each
		].
	].
	self addMorphBack: aMorph
