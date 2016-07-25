drawing
drawOn: aCanvas
	"Draw with the current alpha
	Can't do simple way since BitBlt rules are dodgy!."
	
	aCanvas fillRectangle: self bounds fillStyle: self fillStyle borderStyle: self borderStyle.
	(self cachedForm width = 0 or: [self cachedForm height = 0]) ifTrue: [^self].
	self layout == #tiled
		ifTrue: [aCanvas fillRectangle: self innerBounds fillStyle: (AlphaInfiniteForm with: self cachedForm)]
		ifFalse: [aCanvas clipBy: self innerBounds during: [:c |
					c translucentImage: self cachedForm at: self layoutPosition]]