printing
printElementsOn: aStream
	| element |
	aStream nextPut: $(.
	base to: size do: [:index | element := self at: index. aStream print: element; space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)
