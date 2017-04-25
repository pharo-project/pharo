building
compose  
	| tabulator |
	tabulator := self tabulator.
	tabulator row: #main.
	tabulator transmit to: #main; andShow: [ :aComposite | 
		aComposite tabulator with: [:browser | 
			browser title: 'Variables'.
			browser row: #variables.
			browser transmit 
				to: #variables port: #entity; 
				andShow: [:a | 
					self variablesIn: a ].
			browser transmit 
				from: #variables port: #entity; 
				to: #variables port: #selection; 
				when: [ :aContext | self debugger selectedVariableName notNil and: [aContext notNil] ];
				transformed: [ :aContext | self selectedVariableForContext: aContext ].
			browser transmit from: #variables; toOutsidePort: #selection; transformed: [ :aNode | aNode value ] ].
		aComposite tabulator with: [:browser | 
			browser title: 'Evaluator'.
			browser row: #evaluator.
			browser transmit to: #evaluator; andShow: [:a | 
				self contextEvaluatorIn: a. ].
			browser transmit from: #evaluator; toOutsidePort: #selection ].
		
		aComposite onChangeOfPort: #activePresentation act: [ :presentation |
			(presentation pane port: #activePresentation) value 
				ifNotNil: [ :activePresentation | 
					self debugger cache at: #activePresentationTitle put: activePresentation title ] ] ].
	tabulator transmit from: #main; toOutsidePort: #selection.