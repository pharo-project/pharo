*Polymorph-Widgets
tabKey: event
	"Check for tab key activity and change focus as appropriate."

	event controlKeyPressed ifFalse: [
		event keyCharacter = Character tab ifTrue: [
			event shiftPressed
				ifTrue: [self navigateFocusBackward]
				ifFalse: [self navigateFocusForward].
			^true]].
	^false