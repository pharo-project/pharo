drawing
drawOn: aCanvas
	| prev right font lines visibleRectangle tcolor left |
	super drawOn: aCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	right := self bounds right.
	left := self bounds left.
	font := self fontToUse.
	prev := nil.
	lines := self lines.
	tcolor := self textColor.
	aCanvas
		clipBy: self bounds
		during: [ :clippedCanvas | 
			| backBnd |
			clippedCanvas fillRectangle: self verticalSeparatorBounds color: self verticalSeparatorColor.
			visibleRectangle := clippedCanvas clipRect.
			(self lineIndexForPoint: visibleRectangle topLeft) to: (self lineIndexForPoint: visibleRectangle bottomRight) do: [ :i | 
				| line |
				line := lines at: i.
				(self selectionStart == self selectionStop and: [self selectionStop textLine lineNumber = line lineNumber])
					ifTrue: [ 
						backBnd := self bounds.
						backBnd := (backBnd left) @ line top corner: self verticalSeparatorBounds left @ line bottom.
						backBnd := backBnd translateBy: 0 @ (self offset y negated + self bounds top).
						clippedCanvas fillRectangle: backBnd color: self selectionColorToUse ].
				prev = line lineNumber
					ifFalse: [ 
						| lineMark lineBnd ygap |
						ygap := ((line lineHeight - font height) // 2 - 1) rounded.
						lineMark := line lineNumber asString.
						self side = #left
							ifTrue: [ 
								| width |
								width := (self fontToUse widthOfString: lineMark) + self horizontalGapAfter.
								lineBnd := (right - width) @ (line bottom - font height - ygap) corner: (right + width) @ line bottom ]
							ifFalse: [ lineBnd := (left + self horizontalGapBefore) @ (line bottom - font height - ygap) corner: right @ line bottom ].
						lineBnd := lineBnd translateBy: 0 @ (self offset y negated + self bounds top).
						clippedCanvas
							drawString: lineMark
							in: lineBnd
							font: font
							color: tcolor.
						prev := line lineNumber ] ] ]