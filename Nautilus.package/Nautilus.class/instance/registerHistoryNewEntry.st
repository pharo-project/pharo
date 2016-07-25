history
registerHistoryNewEntry
	| lastEntry wrapper |
	
	self browsingHistory isPaused ifTrue: [ ^ self ].
	wrapper := NautilusHistoryEntry with: self.
	self browsingHistory entries ifNotEmpty: [ :entries |
		lastEntry := entries last key.
		( lastEntry selectedClass isNil 
		or: [ ( wrapper selectedClass notNil 
			and: [ wrapper selectedMethod = lastEntry selectedMethod 
			and: [ wrapper selectedClass theNonMetaClass = lastEntry selectedClass theNonMetaClass ] ] ) ] ) 
				ifTrue: [ ^ self ] ].
	
	self browsingHistory add: wrapper.
	self historyChanged.