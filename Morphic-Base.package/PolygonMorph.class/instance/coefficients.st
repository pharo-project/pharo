smoothing
coefficients
	curveState ifNotNil: [^ curveState at: 1].
	^ self vertices size < 1 
		ifTrue: [ self  ]
		ifFalse: [ self coefficientsForMoreThanThreePoints ].  