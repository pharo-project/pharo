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
	textModel announcer when: RubEditsStateChangedInModel send: #textChanged to: self.
	stringMorph := '' asStringMorph.
	stringMorph font: (LogicalFont familyName: self defaultFontFamiliyName pointSize: 16)