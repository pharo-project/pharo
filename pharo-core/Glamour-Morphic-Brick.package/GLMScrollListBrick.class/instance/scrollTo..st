scrolling
scrollTo: anInteger
	|brick index|
	self band doNotLayoutDuring: [ self band removeAllBricks ].
	self moveBandTop.
	visibleIndex := nil.
	
	self model count > 0 ifTrue: [
		index := (anInteger max: 1) min: self model count.
		brick := self getMorphAt: index recycled: nil.
		brick 	withWidth: self band width.
		brick 	doLayout.
	
		brick brickBounds: ((0@(0 max: ((self height - brick height) / 2))) extent: brick extent).
		self addBrickBackNoLayout: brick ].
	
	self update.
	self changed
	
	
	
	