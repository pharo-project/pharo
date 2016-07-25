accessing
itemAt: aValue ifAbsent: exceptionBlock
	self classPool
		associationsDo: [ :assoc | 
			aValue = assoc value value
				ifTrue: [ ^ assoc key ] ].
	^ exceptionBlock value