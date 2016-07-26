composing
composedWithLocal: aTransformation
	"Return the composition of the receiver and the local transformation passed in"
	aTransformation isMatrixTransform2x3 ifFalse:[^super composedWithLocal: aTransformation].
	^self composedWithLocal: aTransformation asMatrixTransform2x3 into: self class new