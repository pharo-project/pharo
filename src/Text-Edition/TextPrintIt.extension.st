Extension { #name : 'TextPrintIt' }

{ #category : '*Text-Edition' }
TextPrintIt >> actOnClick: anEvent for: anObject in: paragraph editor: editor [

	| result |
	result := Smalltalk compiler receiver: anObject; evaluate: evalString.
	InformativeNotification signal: result printString.
	^ true
]
