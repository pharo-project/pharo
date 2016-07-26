examples
gtExampleListClassMethods
	<gtExample>
	<label: 'Phlow list displaying all methods of an object '>

	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b |
			b list
			title: 'Methods of Object';
			display: #yourself;
			format: [ :compiledMethod | compiledMethod gtDisplayString ];
			styled: [ :brick | brick height: 30 ];
			entity: Object methods ]