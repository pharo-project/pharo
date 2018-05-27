Extension { #name : #TextPrintIt }

{ #category : #'*Text-Edition' }
TextPrintIt >> actOnClick: anEvent for: anObject in: paragraph editor: editor [

	| result |
	result := Smalltalk compiler receiver: anObject; evaluate: evalString.
	UIManager inform: result printString.
	^ true 
]
