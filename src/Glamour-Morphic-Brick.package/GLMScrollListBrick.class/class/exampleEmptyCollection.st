examples
exampleEmptyCollection
	|adaptor|
	adaptor := nil.
	adaptor := GLMBrickListModel new
		sizeLogic: [ 0 ];
		morphAtLogic: [ :index :morph | 
			(morph ifNil: [ GLMLabelBrick new ])
				color: Color random muchLighter;
				text: index asString, ' of ', adaptor count asString;
				height: 30;
				textPosition: #center;
				hSpaceFill;
				yourself ].

	^ GLMFoundationBrick new
			brickRoot: (self new model: adaptor);
			extent: 300@400;
			openInWindow