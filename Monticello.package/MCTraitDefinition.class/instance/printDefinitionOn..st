printing
printDefinitionOn: stream
	stream nextPutAll: 'Trait named: #', self className;
		 cr;
		 tab;
		 nextPutAll: 'uses: ';
		 nextPutAll: self traitCompositionString;
		 cr;
		 tab;
		 nextPutAll: 'category: ';
		 store: self category asString
