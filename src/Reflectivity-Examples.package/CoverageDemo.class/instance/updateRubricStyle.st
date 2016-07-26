styling
updateRubricStyle

	codePanelMorph widget submorphs do:[:card| |conf ast|
			
		"delete previous styling"
		conf := RubConfigurationChange new.
		conf configurationBlock: 
			[:text | text segments copy do:#delete].
		card textModel announce: conf.
			
		"add new styling"
		ast := card compiledMethod ast.
		(ast children anySatisfy: [ :node| node hasBeenExecuted])
			ifTrue:[
				ast acceptVisitor: (CoverageRubricStyler new textModel: card textModel)
			]
	]