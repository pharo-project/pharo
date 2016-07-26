examples
gtExampleListWithCustomItems
	<gtExample>
	<label: 'Phlow list with custom items'>

	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b |
			b list
				title: 'Example with custom items';
				display: [ :x | x + 3 ];
				format: [ :anInteger | anInteger asString ];
				styled: [ :aBrick | aBrick textPosition: #center; textColor: Color random ];
				entity: (1 to: 100) ]