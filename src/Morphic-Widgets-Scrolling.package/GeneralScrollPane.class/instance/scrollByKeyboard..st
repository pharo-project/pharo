event handling
scrollByKeyboard: event 
	"If event is ctrl+up/down then scroll and answer true."
	
	|sb|
	sb := event commandKeyPressed
		ifTrue: [self hScrollbar]
		ifFalse: [self vScrollbar].
	(event keyValue = 30 or: [event keyValue = 11]) ifTrue: [
		sb scrollUp: 3.
		^true].
	(event keyValue = 31 or: [event keyValue = 12])ifTrue: [
		sb scrollDown: 3.
		^true].
	^false