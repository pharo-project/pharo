accessing
openOrClosePhrase
	| curveName |
	curveName := (self isCurve
				ifTrue: ['curve']
				ifFalse: ['polygon']) translated.
	^ closed
		ifTrue: ['make open {1}' translated format: {curveName}]
		ifFalse: ['make closed {1}' translated format: {curveName}]