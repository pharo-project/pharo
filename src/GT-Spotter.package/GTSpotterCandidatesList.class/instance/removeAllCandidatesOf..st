adding/removing
removeAllCandidatesOf: aProcessor
	|processorLink head tail current|
	
	processorLink := self getProcessorLink: aProcessor.
	
	(processorLink isEmpty) ifTrue: [ ^ self ].
	
	head := processorLink headLink.
	tail := processorLink tailLink.
		
	(head = tail) ifTrue: [
		self candidates removeCycledLink: head.
		processorLink clear. ].
	
	current := head.
	[ current = tail ] whileFalse: [ |nextCurrent|
		nextCurrent := current nextLink.
		self candidates removeCycledLink: current.
		current := nextCurrent ].
	
	self candidates removeCycledLink: current.
	
	processorLink clear.
	
	
	
	
	
	