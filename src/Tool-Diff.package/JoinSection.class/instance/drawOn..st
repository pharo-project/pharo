drawing
drawOn: aCanvas
	"Draw the join on the given canvas."
	
	|v bc|
	(self src color isTransparent and: [self dst color isTransparent])
		ifTrue: [^self].
	v := self shape vertices.
	aCanvas
		drawPolygon: v
		fillStyle: (self fillStyleFor: self shape bounds).
	(self borderWidth > 0 and: [self borderColor isTransparent not]) ifTrue: [
		bc := self borderColorToUse.
		aCanvas
			line: v first + (0@self borderWidth // 2)
			to: v second + (-1@self borderWidth // 2)
			width: self borderWidth
			color: bc;
			line: v third - (1@(self borderWidth // 2))
			to: v fourth - (0@(self borderWidth // 2))
			width: self borderWidth
			color: bc]