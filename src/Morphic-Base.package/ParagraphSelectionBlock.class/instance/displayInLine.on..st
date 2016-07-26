displaying
displayInLine: line on: aCanvas 
	"Display myself in the passed line."
	| startIdx stopIdx leftX rightX |
	startIdx := first stringIndex.
	stopIdx := last stringIndex.
	(stopIdx < line first or: [startIdx > (line last + 1)]) ifTrue: [^self].	"No selection on this line"
	(stopIdx = line first and: [last textLine ~= line]) ifTrue: [^self].	"Selection ends on line above"
	(startIdx = (line last + 1) and: [last textLine ~= line]) ifTrue: [^self].	"Selection begins on line below"
	leftX := (startIdx < line first 
		ifTrue: [line] 
		ifFalse: [first]) left.
	rightX := (stopIdx > (line last + 1) or: [stopIdx = (line last + 1) and: [last textLine ~= line]])  
				ifTrue: [line right]
				ifFalse: [last left].
	aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom) color: color