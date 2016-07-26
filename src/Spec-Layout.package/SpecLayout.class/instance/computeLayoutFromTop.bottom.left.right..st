private
computeLayoutFromTop: top bottom: bottom left: left right: right
	| originY offsetCornerX cornerX offsetOriginY offsetCornerY cornerY originX offsetOriginX |
	
	originX := left isFloat
		ifTrue: [ left ]
		ifFalse: [ 0 ].
	originY := top isFloat
		ifTrue: [ top ]
		ifFalse: [ 0 ].
	cornerX := right isFloat
		ifTrue: [ 1 - right ]
		ifFalse: [ 1 ].
	cornerY := bottom isFloat
		ifTrue: [ 1 - bottom ]
		ifFalse: [ 1 ].
	offsetOriginX := left isInteger
		ifTrue: [ left ]
		ifFalse: [ 0 ].
	offsetOriginY := top isInteger
		ifTrue: [ top ]
		ifFalse: [ 0 ].
	offsetCornerX := right isInteger
		ifTrue: [ right negated ]
		ifFalse: [ 0 ].
	offsetCornerY := bottom isInteger
		ifTrue: [ bottom negated ]
		ifFalse: [ 0 ].
		
	^ { originX@originY. cornerX@cornerY. offsetOriginX@offsetOriginY. offsetCornerX@offsetCornerY }