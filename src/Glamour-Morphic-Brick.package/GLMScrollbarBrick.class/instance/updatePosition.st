scrollbar-updating
updatePosition
	|height top width|
	self scrollPane ifNil: [ ^ self ].
	self scrollPane band height = 0 ifTrue: [
		self bar brickBounds: ((0@0) extent: 0@0).
		^ self changed ].
	
	height := self calculateScrollbarHeight.
	top := self calculateScrollbarTopOffset.
	
	width := (height >= self height)
		ifTrue: [ 0 ]
		ifFalse: [ self width ].
	
	self bar brickBounds: (0@top extent: width@height).
	
	self changed