removing
remove: aGTEventCollector 
	^ mutex critical: [ collectors remove: aGTEventCollector ifAbsent: nil ]