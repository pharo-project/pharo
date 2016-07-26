drawing
drawLinesOn: aCanvas lineColor: lineColor 
	| hasToggle |
	hasToggle := self hasToggle.
	"Draw line from toggle to text"
	self drawLineToggleToTextOn: aCanvas lineColor: lineColor hasToggle: hasToggle.

	"Draw the line from my toggle to the nextSibling's toggle"
	self nextSibling ifNotNil: [ self drawLinesToNextSiblingOn: aCanvas lineColor: lineColor hasToggle: hasToggle ].

	"If I have children and am expanded, draw a line to my first child"
	(self firstChild notNil and: [ self isExpanded ])
		ifTrue: [ self drawLinesToFirstChildOn: aCanvas lineColor: lineColor]