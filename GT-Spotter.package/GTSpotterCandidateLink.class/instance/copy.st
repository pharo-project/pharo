copying
copy
	|link|
	link := self class value: self value.
	link privateProcessorLink: processorLink.
	link privateRenderingProcessorLink: renderingProcessorLink.
	link weight: weight.
	^ link