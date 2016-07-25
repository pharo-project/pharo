running
directCircuit: aCollection

	^ self normalize: (OrderedCollection 
		with: aCollection first model 
		with: aCollection second model)