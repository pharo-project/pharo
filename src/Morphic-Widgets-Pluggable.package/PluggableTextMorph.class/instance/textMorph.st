editor access
textMorph
	^ textMorph ifNil: [ 
		self textMorph: self textMorphClass new.
		textMorph ]