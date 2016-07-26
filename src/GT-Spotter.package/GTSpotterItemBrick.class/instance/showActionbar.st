actions
showActionbar

	self actionbarBrick hasStepModel
		ifFalse: [ self actionbarBrick stepModel: self stepModel ].
	
	self actionbarBrick visible: true