ui requests
requestPassword: aStringOrText 
	"Request for a password.
	Allow for interception with a ProvideAnswerNotification handler.
	Answer nil if the user cancels."
	
	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #default ifTrue: [''] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^modalMorph theme 
		passwordEntryIn: modalMorph
		text: aStringOrText
		title: 'Password Required' translated
		entryText: ''