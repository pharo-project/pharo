initialization
initialize

	super initialize.
	
	pattern := ''.
	
	dataSource := FTEasyListDataSource new
		elements: #();
		display: [ :item |
			String streamContents: [ :stream |			
				stream 
					nextPutAll: item value asString;
					space: (10 - ((stream position *2) min: 8));
					nextPutAll: item key asString
			]
		];
		yourself.
				
	textModel := RubScrolledTextModel new.
	textModel announcer when: RubEditsStateChangedInModel 
		send: #update to: self.
	
	stringMorph := StringMorph contents: '' font: self aquireFont
		
	