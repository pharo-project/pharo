fileIn/Out
definition
	^String streamContents: [:stream |
		stream
			nextPutAll: self name;
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString]