visiting
normalizeAxisValue: value
	| result deadZone |
	result := value asFloat / self axisMaxValue.
	deadZone := self axisDeadZoneThreshold.
	(result between: deadZone negated and: deadZone) ifTrue: [ 
		result := 0.0
	] ifFalse: [
		result < 0 ifTrue: [ 
			result := result + deadZone.
		] ifFalse: [ 
			result := result - deadZone.
		].
		result := result / (1.0 - deadZone).
	].

	^ result min: 1.0 max: -1.0