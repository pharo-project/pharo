accessing
labelMorph: aMorph
	"Set the value of labelMorph"

	labelMorph ifNotNil: [labelMorph delete].
	labelMorph := aMorph.
	labelMorph color: (self theme checkboxButtonLabelNormalFillStyleFor: self label: labelMorph).
	self addMorphBack: aMorph.
	self enabled: self enabled.