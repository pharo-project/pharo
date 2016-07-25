scrollpane-scrolling
scrollByKeyboard: anEvent
	|direction|
	
	(self isKeyboardScrollEvent: anEvent) ifFalse: [^ self ].
	self velocity: (self calculateVelocity: anEvent).
	
	"keycode 30 is Event direction = #up"
	anEvent keyValue = 30 ifTrue: [ direction := #up ].
	"keycode 31 is Event direction = #down"
	anEvent keyValue = 31 ifTrue: [ direction := #down ].
	self scrollBy: velocity direction: direction