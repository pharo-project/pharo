public
printElementsOn: aStream
	| oldPos |
	aStream nextPut: $(.
	oldPos := aStream position.
	self do: [:element | aStream print: element; space].
	aStream position > oldPos ifTrue: [aStream skip: -1 "remove the extra space"].
	aStream nextPut: $)