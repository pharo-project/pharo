events handling
whenTextAreaLostFocus: anAnnounce
	self dialog isInWorld ifFalse: [ ^self ].
	self textArea whenFindTextChanged: RubFindTextChanged new
