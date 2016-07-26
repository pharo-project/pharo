accessing
bundle: aCollection
	recordedEvents flush: [ :eachEvent | aCollection addLast: eachEvent ].
	^ aCollection