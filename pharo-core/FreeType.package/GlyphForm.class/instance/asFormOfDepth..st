converting
asFormOfDepth: d
	| newForm |
	d = self depth ifTrue:[^self].
	newForm := self class extent: self extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (self colormapIfNeededFor: newForm);
		copy: (self boundingBox)
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	newForm 
		offset: offset;
		advance:advance;
		linearAdvance: linearAdvance.
	^newForm