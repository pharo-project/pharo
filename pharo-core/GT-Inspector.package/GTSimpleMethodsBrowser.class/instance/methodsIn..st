building
methodsIn: a
	^ a list
		display: [ :object :class | class methods collect: #asRingDefinition];
		filterOn: [ :text :each | (text , '*') match: each selector ];
		helpMessage:  'Wildcard filtering (e.g., "*pattern*")';
		tags: [ :method | method category ];
		sorted: [ :method1 :method2 | method1 selector < method2 selector ];
		format: [ :method | method selector ];
		morphicSelectionAct: [ :list :object :class | 
			Smalltalk tools browser 
				openOnClass: class 
				selector: list selection selector ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			on: $b
			entitled: 'Browse';
		dynamicActionsOnSelection: [ :list | self methodActionsFor: list selection ]