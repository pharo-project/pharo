actions
diveInCategoryDueTo: anEventType

	self spotterModelDo: [ :aSpotter |
		aSpotter hasSelected
			ifTrue: [ aSpotter currentStep diveInCategoryDueTo: anEventType ] ]