examples - cascades
exampleCascadeIfNil
	^ {true} asOrderedCollection
		size;
		add: (true ifNil: [ false ]);
		yourself