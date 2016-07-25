private events
processEvent: evt 
	"Process a single event. This method is run at high priority."
	| type |
		
	type := evt at: 1.

	"Treat menu events first"
	type = EventTypeMenu
		ifTrue: [
			self processMenuEvent: evt.
			^nil].

	"Tackle mouse events first"
	type = EventTypeMouse
		ifTrue: [
			"Transmogrify the button state according to the platform's button map definition"
			evt at: 5 put: (ButtonDecodeTable at: (evt at: 5) + 1).
			"Map the mouse buttons depending on modifiers"
			evt at: 5 put: (self mapButtons: (evt at: 5) modifiers: (evt at: 6)).

			"Update state for polling calls"
			mousePosition := (evt at: 3) @ (evt at: 4).
			modifiers := evt at: 6.
			mouseButtons := evt at: 5.

			^evt].
	
	
	"Finally keyboard"
	type = EventTypeKeyboard
		ifTrue: [
			"Update state for polling calls"
			modifiers := evt at: 5. 
			^evt].
				
	"Handle all events other than Keyborad or Mouse."
	^evt.
	