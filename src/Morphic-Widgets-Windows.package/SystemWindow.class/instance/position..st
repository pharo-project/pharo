geometry
position: newPos
	| oldPos |
	isCollapsed
		ifTrue: [ oldPos := collapsedFrame origin ]
		ifFalse: [ oldPos := fullFrame origin ].

	super position: newPos.
	isCollapsed
		ifTrue: [collapsedFrame := self bounds]
		ifFalse: [fullFrame := self bounds].

	(oldPos ~= newPos)
		ifTrue: [ self announce: 
					(WindowMoved new 
						oldPosition: oldPos;
						newPosition: newPos;
						window: self)]
