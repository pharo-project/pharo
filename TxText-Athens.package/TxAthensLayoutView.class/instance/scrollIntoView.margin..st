scrolling
scrollIntoView: aTextPosition margin: visibleMargin

	"Scroll the view so, that given position is fully visible inside a view.
	The margin controls the minimal offset from view border and given position.
	"
	
	"Simple case first: position is inside a layout"
	| margin |
	
	margin := visibleMargin asMargin.
	
	(layout interval contains: aTextPosition) ifTrue: [ 
		
		| layoutPos deltaX newXOffset deltaY newYOffset |
	
		layoutPos := self layoutPositionAt: aTextPosition ifOutside: [  self error: 'should not happen' ].
		
		deltaX := layoutPos horizontalOffset - offset x. 
		
		newXOffset := offset x.
		"position to the left from current view"
		deltaX < margin left 
			ifTrue: [ 
				 newXOffset := layoutPos horizontalOffset - margin left ].

		"position to the right from current view"
		deltaX > (self width - margin right)
			ifTrue: [ newXOffset := layoutPos horizontalOffset - self width + margin right  ].
		
		"Just to be sure, check side constraints"
		newXOffset + self width > (layout extent x + margin right) ifTrue: [ 
			newXOffset := layout extent x + margin right - self width ].
		newXOffset < 0 ifTrue: [  newXOffset := 0 ].

		"now check vertical offset(s)"
		newYOffset := offset y.
		deltaY := layoutPos bottomPointInView y + margin bottom - self height. 
		deltaY > 0 ifTrue: [ 
			newYOffset := layoutPos bottomPoint y + margin bottom - self height.
			 ].
		
		deltaY := layoutPos topPoint y.
		deltaY < newYOffset ifTrue: [ newYOffset := deltaY ].
		
		offset := newXOffset @ newYOffset.	
		"make sure layout fully covers the view after scrolling"
		self ensurePointWithinLayout: (0@(offset y + self height)).
		self ensurePointWithinLayout: (0@offset y).
		"compact the layout"
		self compact. 
			
	] ifFalse: [  | repeat |
		repeat := true.
		"position is outside of current layout"
		aTextPosition > layout endPosition ifTrue: [ 
			[repeat and: [aTextPosition > layout endPosition] ] 
				whileTrue: [ repeat := layout extendOneLineDown ]
		] ifFalse: [ 
			[repeat and: [aTextPosition < layout startPosition]] 
				whileTrue: [ repeat := layout extendOneLineUp ]
		 ].
		"repeat"
		repeat ifTrue: [ ^ self scrollIntoView: aTextPosition margin: visibleMargin ]
	].

	self announceViewChanged.
