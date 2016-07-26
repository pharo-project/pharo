private events
generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| position buttons modifiers type trail stamp oldButtons evtChanged |
	#KMBuffer asClassIfPresent: [ :kmBuffer | kmBuffer uniqueInstance clearBuffer].
	evtBuf first = lastEventBuffer first 
		ifTrue: 
			["Workaround for Mac VM bug, *always* generating 3 events on clicks"

			evtChanged := false.
			3 to: evtBuf size
				do: [:i | (lastEventBuffer at: i) = (evtBuf at: i) ifFalse: [evtChanged := true]].
			evtChanged ifFalse: [^nil]].
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue].
	position := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: 
			[lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: 
			[lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: 
			[trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent basicNew 
				setType: type
				startPoint: (self position)
				endPoint: trail last
				trail: trail
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent basicNew 
		setType: type
		position: position
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp