processors
getProcessorLink: aProcessor

	^ self processorLinksMap at: aProcessor ifAbsentPut: [ self addProcessor: aProcessor ]