printing
printCommentOn: aStream indent: indent 
	| thisComment |
	self comment == nil ifTrue: [^ self].
	1 to: self comment size
	   do: [:index |
		index > 1 ifTrue: [aStream crtab: indent].
		aStream nextPut: $".
		thisComment := self comment at: index.
		self printSingleComment: thisComment
			on: aStream
			indent: indent.
		aStream nextPut: $"]