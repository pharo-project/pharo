event-handling
update
	self preventUpdatesDuring: [ 
		self updateInspectorChoice.
		self updateInspectorType.
		self previousButton enabled: self history hasPrevious.
		self nextButton enabled: self history hasNext ]