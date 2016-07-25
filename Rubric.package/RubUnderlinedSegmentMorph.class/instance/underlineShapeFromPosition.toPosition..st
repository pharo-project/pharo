drawing
underlineShapeFromPosition: firstPos toPosition: lastPos
	| shape ygap pos |
	shape := OrderedCollection new.
	ygap := self straight
		ifTrue: [ 0 ]
		ifFalse: [ 1 ].
	shape add: (pos := firstPos x @ (firstPos y + (ygap // 2))).
	[ pos x < (lastPos x min: self right) ]
		whileTrue: [ 
			ygap := ygap negated.
			pos := (pos x + 2 min: self right) @ (pos y + ygap).
			shape add: pos.
			pos := pos + (1 @ 0) ].
	^ shape