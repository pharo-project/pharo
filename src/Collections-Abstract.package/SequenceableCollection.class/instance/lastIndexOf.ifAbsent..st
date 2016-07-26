accessing
lastIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock