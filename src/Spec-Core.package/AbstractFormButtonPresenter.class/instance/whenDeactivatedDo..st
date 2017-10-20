as yet unclassified
whenDeactivatedDo: aBlock
	"This method is used to propagate the event that I have been deactivated"
	
	stateHolder whenChangedDo: [:bool | bool ifFalse: aBlock ]