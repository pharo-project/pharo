protocol
protocolOfSelector: aSelector

	^self protocols 
		detect: [:each | each includesSelector: aSelector ]
		ifNone: [ nil ]