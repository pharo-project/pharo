loading
loadPostloadDirective: aPostloadDirective 

	| block |
	(block :=  aPostloadDirective spec postLoadDoItBlock) ~~ nil
		ifTrue: [
			aPostloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: 
					(Array 
						with: aPostloadDirective loader 
						with: aPostloadDirective spec) ].
			Transcript cr; show: 'Evaluated -> ', aPostloadDirective spec label, ' >> ', aPostloadDirective spec postLoadDoIt value asString]