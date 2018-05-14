Extension { #name : #TextDoIt }

{ #category : #'*Text-Edition' }
TextDoIt >> actOnClick: anEvent for: anObject in: paragraph editor: editor [

	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Smalltalk compiler receiver: anObject; evaluate: evalString.
	^ true 
]
