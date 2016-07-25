accessing
verticalSeparatorBounds
	| bnds  |
	bnds := self bounds.
	^ self side = #left
		ifTrue: [ (bnds topRight -  (self verticalSeparatorWidth @ 0)) corner: bnds bottomRight ]
		ifFalse: [ (bnds topLeft corner: bnds bottomLeft + (self verticalSeparatorWidth @ 0)) ]