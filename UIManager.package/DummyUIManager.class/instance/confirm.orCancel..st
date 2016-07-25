ui requests
confirm: aString orCancel: cancelBlock

	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer |
		^answer == #cancel ifTrue: [cancelBlock value] ifFalse: [answer]].

	self error: 'No user response possible'