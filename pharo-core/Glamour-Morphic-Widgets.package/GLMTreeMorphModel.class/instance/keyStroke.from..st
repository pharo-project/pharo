callbacks
keyStroke: aKeyboardEvent from: aTreeMorph 
	| action |
	action := self allKeystrokeActions detect: [:a | a shortcut = aKeyboardEvent keyCharacter] ifNone: [nil].
	action ifNotNil: [ self announce: (GLMKeyStroke action: action) ]