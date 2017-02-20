scrolling
glmAnimateValue: aValue duration: anInteger callback: aBlock
	
	self animation ifNotNil: #terminate.
	animation := GLMAnimation new
		linear;
		duration: 200;
		from: self scrollValue;
		to: (self convertScrollValue: aValue);
		ensure: [ :b | b updatePaneBounds. aBlock brickValue: b ];
		animation: [ :b :v | b scrollValue: v ];
		animate: self