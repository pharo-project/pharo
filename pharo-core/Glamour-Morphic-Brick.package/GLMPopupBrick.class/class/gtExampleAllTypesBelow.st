examples
gtExampleAllTypesBelow
	<gtExample>
	<label: 'Example of all popup types opening below of anchor brick'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createButton: [ :b | self new openBelowFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beHelp; openBelowFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beError; openBelowFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beSuccess; openBelowFor: b ]);
		marginTop: 35