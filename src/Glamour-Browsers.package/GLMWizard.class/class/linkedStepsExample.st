examples
linkedStepsExample

	"self linkedStepsExample"
	| wizard |
	wizard := GLMWizard new.
	(wizard step: #step1) 
		show: [:a |
			a list
				display: [:aList |
					aList
					].
			];
		name: 'first pane'
		.
	(wizard step: #step2) 
		from: #step1; 
		show: [:a |
			a list
				display: [:aNumber |
					1 to: aNumber
					]
			];
		name: 'second pane'.
	wizard openOn: (1 to: 100).
