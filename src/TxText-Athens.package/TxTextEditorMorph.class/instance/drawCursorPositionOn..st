drawing
drawCursorPositionOn: aCanvas
	"debugging: draw the cursor position (line/col numbers)"
	| pathTransform pos |

	pos := controller cursorPosition asString.
	
	pathTransform := aCanvas pathTransform.
	pathTransform restoreAfter: [
		pathTransform translateX: 0 Y: self height - 20.
		aCanvas setFont: (LogicalFont familyName: 'Arial' pointSize: 16).
		aCanvas setPaint: (Color red alpha:0.5).
		aCanvas drawString: pos ]
	
	