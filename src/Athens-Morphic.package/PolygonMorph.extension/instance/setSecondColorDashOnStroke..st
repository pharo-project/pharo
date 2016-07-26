*Athens-Morphic
setSecondColorDashOnStroke: anAthensStrokePaint
	| d c commonOffset |
	d := self dashedBorder.
	c := d third.
	commonOffset := d size > 3
		ifTrue: [ d fourth ]
		ifFalse: [ 0 ].
	anAthensStrokePaint fillPaint: c.
	anAthensStrokePaint
		dashes:
			{(d second).
			(d first)}
		offset: commonOffset + d second