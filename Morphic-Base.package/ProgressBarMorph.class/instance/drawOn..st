drawing
drawOn: aCanvas
	"Draw the receiver with the fill style for the bar, clipping to the inner bounds."

	| area |
	area := self innerBounds.
	aCanvas fillRectangle: area fillStyle: self fillStyle.
	value > 0
		ifTrue: [ 
			cachedWidth := self completedWidth.
			area := area origin extent: cachedWidth @ area height.
			aCanvas fillRectangle: area fillStyle: self barFillStyle ].
	self borderStyle frameRectangle: self bounds on: aCanvas