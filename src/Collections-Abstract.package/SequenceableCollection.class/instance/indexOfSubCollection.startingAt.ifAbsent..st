accessing
indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first := sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index := 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index := index+1]]].
	^ exceptionBlock value