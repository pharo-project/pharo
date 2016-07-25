visitor-double dispatching
emitMessageNode: aMessageNode

	aMessageNode isCascaded ifFalse: [
		valueTranslator visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | 
		valueTranslator visitNode: each].
	aMessageNode isSuperSend
		ifTrue: [methodBuilder send: aMessageNode selector toSuperOf: self compilationContext getClass]
		ifFalse: [methodBuilder send: aMessageNode selector].
