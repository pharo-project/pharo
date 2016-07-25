*Polymorph-Widgets
mouseWheel: event 
	"Handle a mouseWheel event."
	
	event isUp ifTrue: [ ^ scrollBar scrollUp: 3 ].
	event isDown ifTrue: [ ^ scrollBar scrollDown: 3 ].
	event isLeft  ifTrue: [ ^ hScrollBar scrollLeft: 3 ].
	event isRight  ifTrue: [ ^ hScrollBar scrollRight: 3 ].