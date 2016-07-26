diff
computeChangesFrom: other in: modification
	| additions changes removals copies |
	other hasSlots ifFalse: [ ^ modification additions: self allSlots asArray ].
	
	additions := self allSlots.
	removals := OrderedCollection new.
	copies := modification copies.
	changes := modification changes.
	
	other allSlots do: [ :oldSlot |
		(self popSlot: oldSlot from: additions)
			ifNil: [ removals add: oldSlot ]
			ifNotNil: [ :newSlot |
				newSlot = oldSlot
					ifTrue: [ copies at: newSlot put: oldSlot ]
					ifFalse: [ changes at: newSlot put: oldSlot ]]].
			
	modification
		additions: additions asArray;
		removals: removals asArray.