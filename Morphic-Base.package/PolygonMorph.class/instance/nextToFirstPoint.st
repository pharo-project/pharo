smoothing
nextToFirstPoint
	"For arrow direction"
	self isCurvy
		ifTrue: [curveState
				ifNil: [self coefficients].
			^ curveState second]
		ifFalse: [^ vertices second]