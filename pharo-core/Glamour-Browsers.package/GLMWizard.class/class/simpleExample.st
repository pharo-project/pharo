examples
simpleExample

	"self simpleExample"
	| wizard |
	wizard := GLMWizard new.
	(wizard genericStep: #step)
		show: [:a | 
		a list
			display: [:list | list] 
		] ;
		name: 'Wizard pane';
		size: 2.
	wizard openOn: (1 to: 100).
