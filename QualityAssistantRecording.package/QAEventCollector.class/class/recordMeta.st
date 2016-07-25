actions
recordMeta
	
	[ GTEventCollector new
	category: #qualityAssistantMeta;
	addIfAvailable: {
		#nautilus  -> QASettings nautilusPlugin .
		#inspector -> QASettings inspectorPluggin .
		#spotter   -> QASettings spotterPlugin
		} asDictionary;
	register
	] on: Error
	  do: [ :error | ReSettings showErrors ifTrue: [ error pass ] ]