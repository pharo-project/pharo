browsers
wizard
	"<glmBrowser: 'Wizard' input: '100'>"
	"self new wizard inspect openOn: 100"

	| wizard |
	self flag: 'this example does not work in the example browser'.
	wizard := GLMWizard new.
	(wizard genericStep: #step)
		show: [:a | 
			a list
				display: [:list | 1 to: 100] ];
		name: 'Wizard pane';
		size: 2.
	^ wizard