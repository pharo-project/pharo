writing
writeMetaclassDefinition: definition
	self chunkContents: [:str |
		str	 nextPutAll: definition className;
			nextPutAll: ' class';
			cr; tab.
			definition hasClassTraitComposition ifTrue: [
				str	 nextPutAll: 'uses: ';
					nextPutAll: definition classTraitCompositionString;
					cr; tab].
			str	 nextPutAll: 'instanceVariableNames: ''';
				nextPutAll: definition classInstanceVariablesString;
				nextPut: $']