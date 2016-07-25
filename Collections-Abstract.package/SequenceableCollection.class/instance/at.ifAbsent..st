accessing
at: index ifAbsent: exceptionBlock 
	"Answer the element at my position index. If I do not contain an element 
	at index, answer the result of evaluating the argument, exceptionBlock."

	(index between: 1 and: self size) ifTrue: [^ self at: index].
	^ exceptionBlock value