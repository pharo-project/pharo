updating
updateCursor: scrollViewToCursorPos

	| position textPosition |

	scrollViewToCursorPos ifTrue: [  
		layoutViewMorph 
			scrollIntoView: controller cursorPosition
			margin: CursorEdgeMargin
		 ].

	textPosition := controller cursorPosition.
	position := layoutViewMorph layoutPositionAt: textPosition ifOutside: [  
		^ cursorMorph visible: false.
		].
	cursorMorph visible: self hasFocus.
	cursorMorph layoutPosition: position.
