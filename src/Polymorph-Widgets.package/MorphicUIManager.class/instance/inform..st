ui requests
inform: aStringOrText
	"Display a message for the user to read and then dismiss."
	
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^true].
	GrowlMorph 
		openWithLabel: 'Information' translated 
		contents: aStringOrText