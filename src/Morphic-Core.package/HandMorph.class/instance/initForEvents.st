initialization
initForEvents
	mouseOverHandler := nil.
	lastMouseEvent := MouseEvent basicNew setType: #mouseMove position: 0@0 buttons: 0 hand: self.
	lastEventBuffer := {1. 0. 0. 0. 0. 0. nil. nil}.
	recentModifiers := 0.
	self resetClickState.