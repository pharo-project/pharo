examples
gtExampleAllTypes
	<gtExample>
	<label: 'Example of all popup types'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createButton: [ :b | self new beUpward; openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beUpward; beHelp; openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beUpward; beError; openOn: b ]);
		addBrickBack: (self createButton: [ :b | self new beUpward; beSuccess; openOn: b ]);
		yourself