*GT-SpotterExtensions-Core
spotterForKeysFor: aStep
	<spotterOrder: 15>
	(self hasProperty: #kmDispatcher) ifFalse: [^self].
	self kmDispatcher spotterForKeysFor: aStep