generating
generateClass: aClass source: sourceStream on: aStream
	| offset source matching selectors |
	offset := self
		lineAndColumn: sourceStream contents
		at: sourceStream position.
	sourceStream
		nextPutAll: (source := self convert: aClass definition);
		nextPut: Character lf; nextPut: Character lf.
	(environment definesClass: aClass) ifTrue: [
		matching := rules select: [ :rule |
			(self isClassEnvironment: rule result)
				and: [ rule result includesClass: aClass ] ].
		self generateViolations: matching source: source offset: offset on: aStream ].
	(environment selectorsForClass: aClass) asSortedCollection
		do: [ :selector | self generateClass: aClass selector: selector source: sourceStream on: aStream ]