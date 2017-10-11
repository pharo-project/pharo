enumerating
detectIndex: aBlock ifNone: exceptionBlock

	self doWithIndex: [:each :index | (aBlock value: each) ifTrue: [^ index]].
	^ exceptionBlock value