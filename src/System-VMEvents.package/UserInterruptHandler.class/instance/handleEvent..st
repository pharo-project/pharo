events
handleEvent: evt
	"Interrupt event... evt is shared, so make a copy of it."

	| buf |
	buf := evt shallowCopy.
	(self isKbdEvent: evt)
		ifTrue: [ 
			| keycode modifiers |
			"Check if the event is a user interrupt"
			keycode := buf sixth.
			modifiers := buf fifth.
			(keycode = interruptKey and: [ modifiers anyMask: 16r0E ])
				ifTrue: [ 
					Display deferUpdates: false.
					(Smalltalk hasClassNamed: #SoundService)
						ifTrue: [ (Smalltalk classNamed: #SoundService) default shutDown ].
					self handleUserInterrupt ].
			^ self ]