callbacks
keystroke: anEvent from: aMorph
	| action |

	self deprecated: 'This method is needed only for the text rederer for PluggableTextMorph.'.
	action := self allKeystrokeActions detect: [:a | a shortcut = anEvent keyCharacter] ifNone: [nil].
	action ifNotNil: [ 
		self announce: (GLMKeyStroke action: action).
		^ true ].
	^ false