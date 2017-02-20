building
compose
	self title: 'Raw'.
	self tabulator with: [ :browser |
		browser row: #variables; row: #evaluator.
		browser transmit 
			to: #variables; 
			andShow: [:a | self variablesIn: a ].
		browser transmit 
			to: #evaluator; 
			andShow: [:a | self evaluatorIn: a ].
		browser transmit 
			from: #variables; 
			toOutsidePort: #selection; 
			transformed: [ :aNode | aNode ifNotNil: [aNode value]].
		browser transmit 
			from: #evaluator; 
			toOutsidePort: #selection ]