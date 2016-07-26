event handling
scrollByKeyboard: event 
	"If event is ctrl+up/down then scroll and answer true"
	(event controlKeyPressed or:[event commandKeyPressed]) ifFalse: [^ false].
	event keyValue = 30
		ifTrue: 
			[scrollBar scrollUp: 3.
			^ true].
	event keyValue = 31
		ifTrue: 
			[scrollBar scrollDown: 3.
			^ true].
	^ false