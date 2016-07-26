private
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer the 	result of evaluating the argument, exceptionBlock."
			
	|currentLink index|		
	currentLink := self linkAt: start ifAbsent: [nil].
	index := start.
	[currentLink isNil ] 
		whileFalse: [currentLink value = anElement value ifTrue: [^index].
					currentLink := currentLink nextLink.
					index := index +1].
	^exceptionBlock value