*GT-InspectorExtensions-Core
gtInspectorAnnotatedMethodsIn: composite
	<gtInspectorPresentationOrder: 30> 
	composite table 
		title: 'All';
		showOnly: 50;
		withSmalltalkSearch;
		display: [:anObject | 
			| pragmas |
			pragmas := OrderedCollection new.
			Object withAllSubclassesDo: [:each |
				Pragma withPragmasIn: each do: [:p |
					pragmas add: p]].
			(pragmas groupedBy: #keyword) associations sorted: [ :a :b | 
				a key < b key ]
		];
		column: 'Keyword' evaluated: [ :each | each key ];
		column: 'Instances' evaluated: [ :each | each value size asString ];
		send: #value