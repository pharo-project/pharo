table
tableWithCustomWidth
	<glmBrowser: 'Table with custom width' input: '100'>
	"self new tableWithCustomWidth openOn: 100"
	| wrapper textBlock |	
	wrapper := GLMWrapper new.
	textBlock := [:x | (1 to: x) inject: '' into: [ :s :each | s, each asString]].
	wrapper show: [:a | 
		a table
			display: [ :x | 1 to: x ]; 
			column: 'Large column' evaluated: textBlock width: 400;
			column: 'Small column' evaluated: textBlock width: 50;
			column: 'Default column' evaluated: textBlock;
			column: 'Last column' evaluated: textBlock ].
	^ wrapper