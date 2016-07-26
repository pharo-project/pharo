private
generateMethod: aSelector withMarker: aSymbol forArgs: aNumber binary: aBoolean
	| source node keywords |
	source := String streamContents: [:stream |
		aNumber < 1
			ifTrue: [stream nextPutAll: aSelector]
			ifFalse: [aBoolean
				ifTrue: [
					stream nextPutAll: aSelector; space; nextPutAll: 'anObject']
				ifFalse: [
					keywords := aSelector keywords.
					1 to: aNumber do: [:argumentNumber |
						stream
							nextPutAll: (keywords at: argumentNumber); space;
							nextPutAll: 'arg'; nextPutAll: argumentNumber asString; space]]].
		stream cr; tab; nextPutAll: 'self '; nextPutAll: aSymbol].
	
	node := self class compiler
		source: source;
		class: self class;
		failBlock: [];
		translate.
	^(node generateWithSource) selector: aSelector; yourself