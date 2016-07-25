user interface
vScrollBarValue: scrollValue
	| old |
	old := atBottom.
	self atBottom: scrollValue >= self verticalScrollbarFrontier.
	old ~= atBottom
		ifTrue: [ self buildPanel ]