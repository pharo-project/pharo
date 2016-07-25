notifying
notifyStepAdded: aStep

	self announce: (GTSpotterStepAdded new stepModel: aStep)