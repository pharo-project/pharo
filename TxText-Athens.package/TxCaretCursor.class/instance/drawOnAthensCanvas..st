drawing
drawOnAthensCanvas: aCanvas

	| cursorShape |

	cursorShape := owner layoutViewMorph topLeft + layoutPosition topPointInView extent: 2@ layoutPosition line height.
	
	self isBlinkingVisibleNow ifTrue: [ 
		aCanvas 
			setShape: cursorShape;
			setPaint: self cursorColor;
			draw
		]