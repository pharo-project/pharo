examples
gtExampleList
	<gtExample>
	<label: 'Phlow simple list'>

	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b |
			b list
				title: 'Example';
				display: [ :x | x + 3 ];
				format: [ :anInteger | anInteger asString, ')' ];
				entity: (1 to: 100) ]