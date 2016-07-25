dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue:[^anObject handleKeystroke: self].
	type == #keyDown ifTrue:[^anObject handleKeyDown: self].
	type == #keyUp ifTrue:[^anObject handleKeyUp: self].
	^super sentTo: anObject.