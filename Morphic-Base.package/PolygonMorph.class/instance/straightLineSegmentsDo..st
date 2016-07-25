smoothing
straightLineSegmentsDo: endPointsBlock
	"Emit a sequence of segment endpoints into endPointsBlock.
	Work the same way regardless of whether I'm curved."
	| beginPoint |
	beginPoint := nil.
		vertices do:
			[:vert | beginPoint ifNotNil:
				[endPointsBlock value: beginPoint
								value: vert].
			beginPoint := vert].
		(closed or: [vertices size = 1])
			ifTrue: [endPointsBlock value: beginPoint
									value: vertices first].