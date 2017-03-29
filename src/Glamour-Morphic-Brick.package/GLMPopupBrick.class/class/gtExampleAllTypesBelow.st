examples
gtExampleAllTypesBelow
	<gtExample>
	<label: 'Example of all popup types opening below of anchor brick'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createButton: [ :b | self new openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beHelp; openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beError; openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beSuccess; openOn: b ]);
		yourself