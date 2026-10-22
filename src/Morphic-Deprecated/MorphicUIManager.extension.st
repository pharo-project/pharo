Extension { #name : 'MorphicUIManager' }

{ #category : '*Morphic-Deprecated' }
MorphicUIManager >> requestPassword: aStringOrText [
	"Request for a password.
	Allow for interception with a ProvideAnswerNotification handler.
	Answer nil if the user cancels."

	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme
		passwordEntryIn: modalMorph
		text: aStringOrText
		title: 'Password Required'
		entryText: ''
]
