generating
generateClass: aClass selector: aSelector source: sourceStream on: aStream
	| offset source matching |
	offset := self
		lineAndColumn: sourceStream contents
		at: sourceStream position.
	sourceStream
		nextPutAll: (source := self convert: (aClass sourceCodeAt: aSelector));
		nextPut: Character lf; nextPut: Character lf.
	matching := rules select: [ :each | 
		(self isSelectorEnvironment: each result)
			and: [ each result includesSelector: aSelector in: aClass ] ].
	self generateViolations: matching source: source offset: offset on: aStream