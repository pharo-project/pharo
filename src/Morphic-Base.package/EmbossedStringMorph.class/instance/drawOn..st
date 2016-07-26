drawing
drawOn: aCanvas
	"Draw the hi/lowlights too."

	|box|
	self style == #plain
		ifTrue: [^super drawOn: aCanvas].
	box := self bounds.
	(self style == #inset or: [self style == #insetNoHighlight])
		ifTrue: [self style == #insetNoHighlight
					ifFalse: [aCanvas drawString: self contents in: (box translateBy: 1)
								font: self fontToUse color: self color veryMuchLighter].
				aCanvas
					drawString: self contents in: (box translateBy: -1)
					font: self fontToUse color: self color muchDarker;
					drawString: self contents in: box
					font: self fontToUse color: self color]
		ifFalse: [self style == #raisedNoHighlight
					ifFalse: [aCanvas drawString: self contents in: (box translateBy: -1)
								font: self fontToUse color: self color veryMuchLighter].
				aCanvas
					drawString: self contents in: (box translateBy: 1)
					font: self fontToUse color: self color muchDarker;
					drawString: self contents in: box
					font: self fontToUse color: self color]