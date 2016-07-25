printing
newTemplateIn: categoryString
	^String streamContents: [:stream |
		stream
			nextPutAll: self name;
			nextPutAll: ' named: #NameOfTrait';
			cr; tab;
			nextPutAll: 'uses: {}';
			cr; tab;
			nextPutAll: 'category: ';
			nextPut: $';
			nextPutAll: categoryString;
			nextPut: $' ]