tests
testOpenClose

	| model transcript window |
	
	self canRunTest ifFalse: [ ^self skip ].
		
	model := TestableTranscriptRubricOutput new.
	transcript := TranscriptStream new.
	
	transcript addOutput: model.
	window := model openLabel: 'TranscriptUITests'.
	
	self
	assert: model textMorph isNotNil;
	assert: (model textMorph isKindOf: Morph);
	assert: model textMorph class == RubScrolledTextMorph ;
	assert: window class == SystemWindow  ;
	assert: window model == model ;
	assert: (transcript outputs includes: model).
	
	window delete.
	
	self
	assert: window isInWorld not;
	assert: window isTopWindow not ;
	deny: (transcript outputs includes: model) ;
	assert: model noModelStuff .
	
	transcript := nil.
	
	Smalltalk garbageCollect.
	
	self
	assert: model connections asArray isEmpty ;
	assert: model noModelStuff. 