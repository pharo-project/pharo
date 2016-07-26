transforming
preserveStateDuring: aBlock
	| state result |
	state := BalloonState new.
	state transform: transform.
	state colorTransform: colorTransform.
	state aaLevel: self aaLevel.
	result := aBlock value: self.
	transform := state transform.
	colorTransform := state colorTransform.
	self aaLevel: state aaLevel.
	^result