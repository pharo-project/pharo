*Polymorph-Widgets
navigationKey: event
	"Check for tab key activity and change focus as appropriate.
	Must override here rather than in #tabKey: otherwise
	the tab will get passed to the window and change the focus."

	(event keyCharacter = Character tab and: [
		(event anyModifierKeyPressed or: [event shiftPressed]) not]) ifTrue: [^false].
	^super navigationKey: event