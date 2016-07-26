accessing
keyAtIndex: anIndex ifAbsent: aBlock
	(anIndex > 0 and: [anIndex <= self size])
		ifTrue: [^ orderedKeys at: anIndex]
		ifFalse: [^ aBlock value]