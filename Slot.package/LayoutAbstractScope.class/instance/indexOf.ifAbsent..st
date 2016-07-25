enumerating
indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the first occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the 
	result of evaluating the argument, exceptionBlock."

	1 to: self size do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value