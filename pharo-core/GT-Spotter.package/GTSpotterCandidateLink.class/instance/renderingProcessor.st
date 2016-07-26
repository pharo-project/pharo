accessing
renderingProcessor

	renderingProcessorLink ifNil: [ self renderingProcessor: processorLink ].
	^ renderingProcessorLink value