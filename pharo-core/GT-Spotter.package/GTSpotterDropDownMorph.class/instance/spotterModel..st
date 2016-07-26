accessing
spotterModel: aModel 
	spotterModel := aModel.
	
	spotterModel steps do: [ :each | self pushPane: (self buildPane: each) ].
	
	spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded: to: self.