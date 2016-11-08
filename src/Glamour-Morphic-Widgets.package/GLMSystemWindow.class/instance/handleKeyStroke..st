as yet unclassified
handleKeyStroke: anEvent
	| action |
	action := self allKeystrokeActions detect: [:a | 
		anEvent commandKeyPressed and: [ 
			a shortcut = anEvent keyCharacter ]] ifNone: [nil].
	action ifNotNil: [ self announcer announce: (GLMKeyStroke action: action) ]