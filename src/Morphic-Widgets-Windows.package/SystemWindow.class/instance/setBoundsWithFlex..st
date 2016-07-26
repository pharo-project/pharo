resize/collapse
setBoundsWithFlex: newFrame
	"Set bounds from newFrame with origin preserved from global coordinates"

	self isFlexed
		ifTrue: [super bounds: ((owner transform globalPointToLocal: newFrame topLeft)
										extent: newFrame extent)]
		ifFalse: [super bounds: newFrame].