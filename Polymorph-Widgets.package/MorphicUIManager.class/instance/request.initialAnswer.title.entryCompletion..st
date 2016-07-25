ui requests
request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion
	"Create an instance of me whose question is queryString with the given 
	initial answer. Answer the string the user accepts.
	The title serves for the window that is opened
	Answer the empty string if the user cancels.
	Allow for interception with a ProvideAnswerNotification handler."
	
	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme 
		textEntryIn: modalMorph
		text: aStringOrText
		title: aTitle
		entryText: defaultAnswer
		entryCompletion: anEntryCompletion)