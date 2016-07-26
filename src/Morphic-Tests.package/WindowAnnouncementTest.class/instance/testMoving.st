tests - window change
testMoving
	| t oldBounds  event |
	window := SystemWindow labelled: 'foo'.
	t := 0.
	event := nil.

	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowMoved do: [:ann | t := t + 1. event := ann].

	self assert: (t = 0).
	self assert: (event isNil).
	
	"We move the window"
	window position: 50@50.
	self assert: (t = 1).
	self assert: (event oldPosition = oldBounds origin).
	self assert: (event newPosition = (50@50 )).

	"We call position: without moving it actually, nothing should happen"
	window position: 50@50.
	self assert: (t = 1).

	"If we simply resize the window, nothing should happen as well"
	window extent: 50@60.
	self assert: (t = 1).

