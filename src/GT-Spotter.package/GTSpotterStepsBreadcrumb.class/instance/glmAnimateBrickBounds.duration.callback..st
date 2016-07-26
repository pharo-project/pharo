geometry
glmAnimateBrickBounds: aValue duration: anInteger callback: aBlock

	self animation ifNotNil: #terminate.
	animation := GLMAnimation new
		linear;
		duration: anInteger;
		from: self band brickBounds left;
		to: self band brickBounds left + aValue;
		onCompleted: aBlock;
		animation: [ :b :v | b changedWith: [ b translateXTo: v asInteger; data: b brickBounds left ] ];
		animate: self band