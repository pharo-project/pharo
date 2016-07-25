examples
genericStepWithGenericSizeExample

	"self genericStepWithGenericSizeExample"
	| wizard |
	wizard := GLMWizard new.
	(wizard genericStep: #step)
		show: [:a | 
		a list
			display: [:aNumber | 1 to: aNumber] 
		] ;
		name: 'Wizard pane';
		size: [:aNumber | aNumber].
	wizard openOn: 4.
