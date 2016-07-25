ui
gtPagesIn: composite
	<gtInspectorPresentationOrder: 0>
	composite list 
		title: 'Pages';
		display: [ self pages ];
		format: #gtDisplayString;
		filterOn: [ :text :each | text match: each content ]