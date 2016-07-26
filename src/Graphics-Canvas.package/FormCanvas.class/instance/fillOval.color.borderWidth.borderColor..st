drawing-ovals
fillOval: r color: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	"draw the border of the oval"
	rect := (r translateBy: origin) truncated.
	(borderWidth = 0 or: [borderColor isTransparent]) ifFalse:[
		self setFillColor: borderColor.
		(r area > 10000 or: [fillColor isTranslucent]) 
			ifTrue: [port frameOval: rect borderWidth: borderWidth]
			ifFalse: [port fillOval: rect]]. "faster this way"
	"fill the inside"
	fillColor isTransparent ifFalse:
		[self setFillColor: fillColor.
		port fillOval: (rect insetBy: borderWidth)].
