Extension { #name : #DisplayScreen }

{ #category : #'*Deprecated10' }
DisplayScreen class >> hostWindowTitle: aString [

	self
		deprecated:
		'Use World worldState worldRenderer windowTitle: aTitle instead'
		transformWith: '`@rec hostWindowTitle: `@arg'
			-> 'self currentWorld worldState worldRenderer windowTitle: `@arg'.

	self currentWorld worldState worldRenderer windowTitle: aString
]
