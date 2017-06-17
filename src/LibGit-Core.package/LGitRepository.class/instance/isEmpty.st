testing
isEmpty
	self exists ifFalse: [ ^ true ].
	
	^ self repository_is_empty: self