private events
processEventsFromQueue: anEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny |
	ActiveEvent ifNotNil: 
			["Meaning that we were invoked from within an event response.
		Make sure z-order is up to date"

			self mouseOverHandler processMouseOver: lastMouseEvent].
	hadAny := false.
	[anEventQueue isNotNil and: [(evtBuf := anEventQueue nextEvent) isNotNil]] whileTrue: 
			[evt := nil.	"for unknown event types"
			type := evtBuf first.
			type = EventTypeMouse ifTrue: [recentModifiers := evtBuf sixth. evt := self generateMouseEvent: evtBuf].
			type = EventTypeKeyboard 
				ifTrue: [recentModifiers := evtBuf fifth. evt := self generateKeyboardEvent: evtBuf].
			type = EventTypeDragDropFiles 
				ifTrue: [evt := self generateDropFilesEvent: evtBuf].
			type = EventTypeWindow
				ifTrue:[evt := self generateWindowEvent: evtBuf].				
			"All other events are ignored"
			(type ~= EventTypeDragDropFiles and: [evt isNil]) ifTrue: [^self].
			evt isNil 
				ifFalse: 
					["Finally, handle it"

					self handleEvent: evt.
					hadAny := true.

					"For better user feedback, return immediately after a mouse event has been processed."
					(evt isMouse and: [evt isMouseWheel not]) ifTrue: [^self]]].
	"note: if we come here we didn't have any mouse events"
	mouseClickState notNil 
		ifTrue: 
			["No mouse events during this cycle. Make sure click states time out accordingly"

			mouseClickState handleEvent: lastMouseEvent asMouseMove from: self].
	hadAny 
		ifFalse: 
			["No pending events. Make sure z-order is up to date"

			self mouseOverHandler processMouseOver: lastMouseEvent]