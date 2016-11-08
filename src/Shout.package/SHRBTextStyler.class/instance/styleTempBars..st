private
styleTempBars: aSequenceNode
	| tempBarAttribute |
	tempBarAttribute := aSequenceNode scope isMethodScope
		ifTrue: [ #methodTempBar ]
		ifFalse: [ #blockTempBar ].
	aSequenceNode leftBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ].
	aSequenceNode rightBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ]