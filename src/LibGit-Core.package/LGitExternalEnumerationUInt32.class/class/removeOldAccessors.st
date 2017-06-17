class initialization
removeOldAccessors
	declaration ifNil: [ ^ self ].
	
	declaration keys do: [ :selector |
		self class removeSelector: selector ]