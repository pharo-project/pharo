utilities
nextChar
	[| evt type | 
		(Delay forMilliseconds: 20) wait.
		Sensor nextEvent
		ifNotNil: [:evtBuf | 
			type := evtBuf first.
			type = EventTypeKeyboard
				ifTrue: [evt := World currentHand generateKeyboardEvent: evtBuf.
					evt isKeystroke
						ifTrue: [| c |
							c := evt keyCharacter.
							(c asciiValue < 128
									or: [c isSeparator
											or: [c isSpecial
													or: [c = Character backspace]]])
								ifTrue: [Sensor flushKeyboard.
									^ evt keyCharacter]]]]] repeat