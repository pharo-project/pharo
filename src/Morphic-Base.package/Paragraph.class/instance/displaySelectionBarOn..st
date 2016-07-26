display
displaySelectionBarOn: aCanvas 
	| visibleRectangle line |
	selectionStart ifNil: [^ self].
	selectionStop ifNil: [^ self].
	visibleRectangle := aCanvas clipRect.
	selectionStart textLine = selectionStop textLine ifFalse: [^self].
	line := selectionStart textLine.
	aCanvas fillRectangle: (visibleRectangle left @ line top corner: visibleRectangle right @ line bottom) color: self selectionBarColor