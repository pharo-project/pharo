initialize
withText: aStringOrText label: title 
	
	self title: title.
	self 
		addMorph: (RubScrolledTextMorph new 
				setText: aStringOrText asString;
				yourself)
		fullFrame: LayoutFrame identity.						
	self themeChanged.
