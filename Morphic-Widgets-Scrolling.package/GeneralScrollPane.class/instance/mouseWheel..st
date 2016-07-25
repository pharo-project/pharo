event handling
mouseWheel: event 
	"Handle a mouseWheel event."
	
	(self scrollTarget handlesMouseWheel: event)
		ifTrue: [^self scrollTarget mouseWheel: event]. "pass on"
	
	event isUp ifTrue: [ ^ vScrollbar scrollUp: 3 ].
	event isDown ifTrue: [ ^ vScrollbar scrollDown: 3 ].
	event isLeft  ifTrue: [ ^ hScrollbar scrollLeft: 3 ].
	event isRight  ifTrue: [ ^ hScrollbar scrollRight: 3 ].