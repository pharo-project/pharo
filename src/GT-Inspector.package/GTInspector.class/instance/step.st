stepping
step
	self window ifNil: [ ^ self ].
	self window isActive ifFalse: [ ^ self ].
	
	self wantsAutomaticRefresh ifTrue: [
		self requestRefresh ]