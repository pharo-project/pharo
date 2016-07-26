initializing
resetQuickly
	" we assume the thorough reset was already performed if there are no examples in here "
	examples isEmptyOrNil ifTrue: [ ^ self ].
	self reset