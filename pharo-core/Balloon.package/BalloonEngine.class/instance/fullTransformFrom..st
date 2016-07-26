accessing
fullTransformFrom: aMatrix
	| m |
	m := self aaTransform composedWith: aMatrix.
	"m offset: m offset + destOffset."
	^m