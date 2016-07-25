examples
gtExampleAllTypes
	<gtExample>
	<label: 'Example of all popup types'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createButton: [ :b | self new openFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beHelp; openFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beError; openFor: b ]);
		addBrickBack: (self createButton: [ :b | self new beSuccess; openFor: b ]);
		marginTop: 35;
		yourself