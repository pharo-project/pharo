accessing
contentsOfArea: aRectangle into: aForm
	self apply: [ :c |
		c contentsOfArea: aRectangle into: aForm ].
	^aForm