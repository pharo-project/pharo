adding/removing
addProcessor: aProcessor
	|processorLink|
	
	processorLink := aProcessor asSpotterProcessorLink.
	"linking processor with it's link"
	self processorLinksMap add: (aProcessor -> processorLink).
	self processors addLast: processorLink.
	
	^ processorLink