accessing
renderingProcessor: aProcessorLink

	renderingProcessorLink ifNil: [ renderingProcessorLink := aProcessorLink processor asSpotterProcessorLink ].
	processorLink ifNil: [ processorLink := aProcessorLink ]