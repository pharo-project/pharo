ui requests
confirm: aStringOrText orCancel: cancelBlock
	"Put up a question dialog (with cancel) with the text queryString.
	Answer true if the response is yes, false if no.
	Answer the value of the cancel block if cancelled.
	This is a modal question--the user must respond yes or no or cancel."
	
	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #cancel ifTrue: [cancelBlock value] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme 
		questionIn: modalMorph
		text: aStringOrText
		title: 'Question' translated) ifNil: cancelBlock