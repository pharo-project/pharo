actions
diveInDueTo: anEventType

	self spotterModelDo: [ :aSpotter |
		aSpotter hasSelected
			ifTrue: [ aSpotter currentStep diveInDueTo: anEventType ] ]