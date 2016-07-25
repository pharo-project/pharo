private
aboutToBeUnplugged

	self paragraph ifNotNil: [ :paragraph |
		paragraph textArea announcer unsubscribe: self 
	]