accessing
modificationAt: idx
	idx > modificationMap size 
		ifTrue: [ ^ slotShift ].
	^ modificationMap at: idx