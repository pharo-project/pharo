events
localHandleKeystroke: evt
	"Answer whether we locally handle the keyStroke event.
	Disregard tabs for now."
	
	(self editView keystrokeFromTextMorph: evt)
		ifTrue: [^ true].
	(self autoAccept and: [evt keyCharacter = Character cr and: [self crAction notNil]])
		 ifTrue: [(self crAction isKindOf: MessageSend)
				ifTrue: [ self crAction value]
				ifFalse: [self crAction value: self text]..
			^ true].
	evt keyCharacter = Character tab ifTrue: [
		evt shiftPressed
			ifTrue: [(self editView respondsTo: #navigateFocusBackward)
						ifTrue: [self editView navigateFocusBackward]]
			ifFalse: [(self editView respondsTo: #navigateFocusForward)
						ifTrue: [self editView navigateFocusForward]].
		^true].
	^false