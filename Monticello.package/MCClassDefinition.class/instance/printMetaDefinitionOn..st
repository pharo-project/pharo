printing
printMetaDefinitionOn: stream
	stream 
		nextPutAll: self className;
		nextPutAll: ' class'; cr; tab.
	self hasClassTraitComposition ifTrue: [
		stream
			nextPutAll: 'uses: ';
			nextPutAll: self classTraitCompositionString;
			cr; tab ].
	stream
		nextPutAll: ' instanceVariableNames: '; 
		store: self classInstanceVariablesString