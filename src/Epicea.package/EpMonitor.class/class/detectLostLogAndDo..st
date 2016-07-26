accessing
detectLostLogAndDo: aBlock

	| lastLog |
	lastLog := self loadCurrentImageLogs ifEmpty: [ ^self ] ifNotEmpty: [:logs | logs first].
	(self current log store isPointsTo: lastLog store) ifTrue: [ ^self ].
	lastLog includesCodeChanges ifFalse: [ ^self ].
	
	^aBlock value: lastLog