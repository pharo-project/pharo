building
buildPane: aStepModel
	|pane|
	
	pane := GTSpotterResultsBrick new stepModel: aStepModel.
	self brickThemer spotterThemer resultsPaneStyleFor: pane.
	^ pane