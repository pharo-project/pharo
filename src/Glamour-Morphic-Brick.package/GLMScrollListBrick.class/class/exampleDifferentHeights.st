examples
exampleDifferentHeights
	|adaptor|
	adaptor := nil.
	adaptor := GLMBrickListModel new
		sizeLogic: [ 50 ];
		morphAtLogic: [ :index :morph |
			(morph ifNil: [ GLMLabelBrick new ])
				color: Color random muchLighter;
				text: index asString, ' of ', adaptor count asString;
				textPosition: #center;
				enable;
				height: [ :b | (((index % 10)*10 + 30) +  (1000 * (2 / b width))) asInteger ];
				hSpaceFill;
				yourself ].

	^ GLMFoundationBrick new
		brickRoot: (self new model: adaptor);
		extent: 800@400;
		openInWindow