building
exampleListIn: a
	^ a list
		title: 'Examples';
		display: [ :exampleClass | exampleClass allExamples ];
		sorted: [ :x :y | (x argumentAt: 1) < (y argumentAt: 1) ];
		format: [ :each | each argumentAt: 1 ]