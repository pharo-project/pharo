actions
recordMeta
	
	[ GTEventCollector new
	category: #qualityAssistantMeta;
	addIfAvailable: {
		#nautilus  -> QASettings nautilusPlugin .
		#inspector -> ReSettings inspectorPluggin .
		#spotter   -> ReSettings spotterPlugin
		} asDictionary;
	register
	] on: Error
	  do: [ :error | ReSettings showErrors ifTrue: [ error pass ] ]