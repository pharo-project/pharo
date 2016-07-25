printing
printElementsOn: aStream
	| noneYet |
	aStream nextPut: $(.
	noneYet := true.
	self associationsDo: [ :each | 
			noneYet
				ifTrue: [ noneYet := false ]
				ifFalse: [ aStream space ].
			aStream print: each].
	aStream nextPut: $)