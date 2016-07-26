actions
diveInCategory: aProcessor dueTo: anEventType
	"dive in specified aCategory"

	self spotter
		requestNextCategoryStepWith: (self candidates getProcessorLink: aProcessor) for: #diveInCategory;
		notifyDiveInCategory: aProcessor for: anEventType