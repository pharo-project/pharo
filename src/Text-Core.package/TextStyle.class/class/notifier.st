accessing
notifier

	^ notifier ifNil: [ notifier := Model new ]