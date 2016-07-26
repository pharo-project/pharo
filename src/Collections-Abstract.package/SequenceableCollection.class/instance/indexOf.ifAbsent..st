accessing
indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the first occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the 
	result of evaluating the argument, exceptionBlock."

	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock