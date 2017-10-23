event handling
handleKeystroke: anEvent 
	"System level event handling."
	| pasteUp |

	anEvent wasHandled
		ifTrue: [^ self].
	self allowsKeymapping
		ifTrue: [ self dispatchKeystrokeForEvent: anEvent ] .
	anEvent wasHandled
		ifTrue: [
			"We need to check for completion here because otherwise it will try to handle the keystroke, 
			even if it was already processed."
			(Smalltalk tools hasToolNamed: #codeCompletion) 
				ifTrue: [ Smalltalk tools codeCompletion uniqueInstance closeMenu ].
			^ self].
	(self handlesKeyStroke: anEvent)
		ifFalse: [^ self].
	self keyStroke: anEvent.
	anEvent wasHandled: true.