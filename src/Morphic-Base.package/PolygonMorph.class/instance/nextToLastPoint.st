smoothing
nextToLastPoint
	"For arrow direction"
	self isCurvy
		ifTrue: [curveState
				ifNil: [self coefficients].
			^ curveState third]
		ifFalse: [^ vertices at: vertices size - 1]