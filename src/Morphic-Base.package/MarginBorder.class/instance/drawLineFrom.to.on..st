accessing
drawLineFrom: startPoint to: stopPoint on: aCanvas
	"Reduce the width by the margin."
	
	| lineColor |
	lineColor := (stopPoint truncated quadrantOf: startPoint truncated) > 2 
				ifTrue: [self topLeftColor]
				ifFalse: [self bottomRightColor].
	aCanvas 
		line: startPoint
		to: stopPoint 
		width: (self width - self margin max: 0)
		color: lineColor