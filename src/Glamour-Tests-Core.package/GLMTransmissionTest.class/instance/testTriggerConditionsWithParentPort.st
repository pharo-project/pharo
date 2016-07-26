tests
testTriggerConditionsWithParentPort 
	| transmission presentation | 
	transmission := GLMTransmission new. 
	presentation := GLMPresentation new. 
	transmission addActiveOrigin: 
			((GLMPresentationBoundPort new) 
				presentation: presentation; 
				name: #port1; 
				yourself). 
	self assert: (transmission originatesAt: 
					((GLMPresentationBoundPort new) 
						presentation: presentation; 
						name: #port1; 
						yourself)). 
	self assert: (transmission originatesAt: (presentation pane port: #port1))