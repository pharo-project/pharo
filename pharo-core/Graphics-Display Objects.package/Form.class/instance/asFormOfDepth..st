converting
asFormOfDepth: d
	| newForm |
	d = self depth ifTrue:[^self].
	newForm := Form extent: self extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (self colormapIfNeededFor: newForm);
		copy: (self boundingBox)
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	"Special case: For a 16 -> 32 bit conversion fill the alpha channel because it gets lost in translation."
	(self depth = 16 and:[d= 32]) ifTrue:[newForm fillAlpha: 255].
	^newForm