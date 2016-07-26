examples
gtExamplePharoMethod
	<gtExample>
	<label: 'Phlow Pharo method'>

	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b |
			b pharoMethod
				title: #printString;
				display: #sourceCode;
				smalltalkClass: #methodClass;
				entity: Morph>>#bounds ]