*Polymorph-Widgets
navigationKey: event
	"Check for tab key activity and change focus as appropriate.
	Check for menu key to do popup.
	Check for active window naviagation."

	(self world navigationKey: event) ifTrue: [^true].
	(self tabKey: event) ifTrue: [^true].
	(event keyCharacter = Character escape and: [
			event anyModifierKeyPressed]) ifTrue: [
		^ self yellowButtonActivity: false].
	^false