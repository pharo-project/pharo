*GT-Inspector-Examples-UI
gtInspectorGTExamplesIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list 
		title: 'Examples';
		display: [ self gtExamples ];
		icon: [:each | each icon];
		format: [:each | String streamContents: [:s | each gtDisplaySummaryOn: s]];
		in: [ :list | list sorted: [ :a :b | (list format cull: a) < (list format cull: b)] ];
		send: [ :each | each returnValue ];
		when: [ self gtExamples notEmpty ]