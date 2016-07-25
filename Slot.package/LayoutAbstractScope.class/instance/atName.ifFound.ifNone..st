accessing
atName: aName ifFound: foundBlock ifNone: exceptionBlock
	self allSlotsDo: [ :slot | 
		slot name == aName ifTrue: [ ^ foundBlock cull: slot ]].
	^ exceptionBlock value
	