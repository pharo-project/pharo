examples
example
	|adaptor|
	adaptor := nil.
	adaptor := GLMBrickListModel new
		sizeLogic: [ 20 ];
		morphAtLogic: [ :index :morph |
			(morph ifNil: [ GLMLabelBrick new ])
				color: Color random muchLighter;
				text: index asString, ' of ', adaptor count asString;
				textPosition: #center;
				height: 40;
				hSpaceFill;
				yourself ].

	^ GLMFoundationBrick new
		brickRoot: (self new model: adaptor);
		extent: 300@400;
		openInWindow