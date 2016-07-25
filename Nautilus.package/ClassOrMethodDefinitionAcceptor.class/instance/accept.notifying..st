protocol
accept: source notifying: aController

	self model selectedClass isMeta
		ifTrue: [ self acceptClassSide: source notifying: aController ]
		ifFalse: [ self acceptInstanceSide: source notifying: aController ]