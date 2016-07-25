loading
loadPreloadDirective: aPreloadDirective 

	| block |
	(block :=  aPreloadDirective spec preLoadDoItBlock) ~~ nil
		ifTrue: [
			aPreloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: 
					(Array 
						with: aPreloadDirective loader 
						with: aPreloadDirective spec) ].
			Transcript cr; show: 'Evaluated -> ', aPreloadDirective spec label, ' >> ', aPreloadDirective spec preLoadDoIt value asString]