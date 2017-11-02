Extension { #name : #TextClassLink }

{ #category : #'*Text-Edition' }
TextClassLink >> actOnClick: anEvent for: target in: aParagraph editor: anEditor [
	anEvent shiftPressed
		ifFalse: [ anEditor browseClassFrom: self className ]
		ifTrue: [ anEditor referencesTo: self className ].
	^ true
]
