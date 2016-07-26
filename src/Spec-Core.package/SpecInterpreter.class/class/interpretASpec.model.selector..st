protocol
interpretASpec: aPresenter model: aModel selector: aSelector
	| result |
	
	 [ result :=  self 
				private_interpretASpec: aPresenter
				model: aModel
				selector: aSelector ] ensure: [ self hardResetBindings ].
	^ result