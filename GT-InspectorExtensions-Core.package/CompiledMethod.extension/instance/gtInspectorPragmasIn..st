*GT-InspectorExtensions-Core
gtInspectorPragmasIn: composite
	<gtInspectorPresentationOrder: 30> 
	composite tabulator 
		title: 'Pragmas';
		when: [ :x | x pragmas notEmpty ];
		with: [ :t |  
			t row: #pragmas; row: #methods.
			t transmit to: #pragmas; andShow: [:a | 
				a list
					title: 'Pragmas';
					display: [ :method | method pragmas ];
					when: [ :x | x pragmas notEmpty ] ].
			t transmit from: #pragmas; to: #methods; andShow: [:a | 
				a list 
					title: 'Methods';
					display: [ :pragma | 
						| methods | 
						methods := OrderedCollection new.
						Object withAllSubclassesDo: [ :each | 
							methods addAll: ((Pragma allNamed: pragma keyword in: each) collect: #method) ].
						methods ];
					format: #selector ].
			t transmit
				toOutsidePort: #selection;
				from: #methods port: #selection
		];
		startOn: self