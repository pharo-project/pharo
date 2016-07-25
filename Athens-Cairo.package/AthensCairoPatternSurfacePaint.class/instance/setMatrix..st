as yet unclassified
setMatrix: aMatrix

	aMatrix restoreAfter: [
		origin ifNotNil: [ aMatrix translateBy: origin negated ].
		super setMatrix: aMatrix.
	]