ui requests
request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion
	(ProvideAnswerNotification signal: aStringOrText)
		ifNotNil: [:answer | 
			^ answer = #default ifTrue: [ defaultAnswer ] ifFalse: [ answer ]].
		
	^ self nonInteractiveRequest: aStringOrText title: aTitle