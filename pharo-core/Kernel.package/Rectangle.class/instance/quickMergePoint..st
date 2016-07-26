transforming
quickMergePoint: aPoint
	"Answer the receiver if it encloses the given point or the expansion of the
	receiver to do so if it doesn't. "

	| useRcvr minX maxX minY maxY |
	useRcvr := true.
	minX := aPoint x < origin x ifTrue: [useRcvr := false. aPoint x] ifFalse: [origin x].
	maxX := aPoint x >= corner x ifTrue: [useRcvr := false. aPoint x + 1] ifFalse: [corner x].
	minY := aPoint y < origin y ifTrue: [useRcvr := false. aPoint y] ifFalse: [origin y].
	maxY := aPoint y >= corner y ifTrue: [useRcvr := false. aPoint y + 1] ifFalse: [corner y].
	^useRcvr
		ifTrue: [self]
		ifFalse: [minX@minY corner: maxX@maxY]
