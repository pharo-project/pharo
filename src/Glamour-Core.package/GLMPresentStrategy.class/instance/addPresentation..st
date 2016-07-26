accessing-convenience
addPresentation: aPresentation
	|currentPresentations|
	
	currentPresentations  := self presentations.
	self presentationsFactory: [ :composite | 
		currentPresentations do: [ :each | composite add: each ].
		composite add: aPresentation copy ]