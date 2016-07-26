accessing
contentsOfArea: aRectangle into: aForm

	self apply: [ :c |
		(c isKindOf: FormCanvas) ifTrue: [
			c contentsOfArea: aRectangle into: aForm.
			^aForm
		].
	].
	self apply: [ :c |
		c contentsOfArea: aRectangle into: aForm.
		^aForm.
	].
	^aForm