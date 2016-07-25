accessing
findRegex
	^ self paragraph textArea ifNotNil: [ :textArea | textArea findRegex ]