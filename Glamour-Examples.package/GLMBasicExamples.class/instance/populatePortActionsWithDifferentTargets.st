text
populatePortActionsWithDifferentTargets
	<glmBrowser: 'Populate port action with different targets' input: '$a to: $z'>
	"self new populatePortActionsWithDifferentTargets openOn: ($a to: $z)"
	| composer |
	composer := GLMCompositePresentation new.
	composer tabulator with: [:t | 
		t column: #index; column: [:c | c row: #recepient1; row: #recepient2].
		t transmit to: #index; andShow: [:a | 
			a list 
				selectionAct: [:list | (list pane port: #custom1) value: list selection ] on: $1 entitled: 'To recepient 1';
				selectionPopulate: #custom2 on: $2 entitled: 'To recepient 2' with: [:list | list selection ] ].
		t transmit from: #index port: #custom1; to: #recepient1; andShow: [:a | 
			a text title: 'Recepient 1'].
		t transmit from: #index port: #custom2; to: #recepient2; andShow: [:a | 
			a text title: 'Recepient 2'].
	].
	^ composer