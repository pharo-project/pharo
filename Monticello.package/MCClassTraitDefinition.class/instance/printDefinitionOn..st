printing
printDefinitionOn: stream
	stream
		nextPutAll: self baseTrait;
		nextPutAll: ' classTrait';
		cr;
		tab;
		nextPutAll: 'uses: ';
		nextPutAll: self classTraitCompositionString