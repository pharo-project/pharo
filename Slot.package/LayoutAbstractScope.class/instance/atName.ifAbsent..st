accessing
atName: aName ifAbsent: aBlock
	self allSlotsDo: [ :slot | 
		slot name == aName ifTrue: [ ^ slot ]].
	^ aBlock value