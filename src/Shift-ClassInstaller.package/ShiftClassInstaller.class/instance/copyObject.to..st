copying
copyObject: oldObject to: newClass
	| newObject |
	
	newObject := newClass isVariable
		ifTrue: [ newClass basicNew: oldObject size ]
		ifFalse: [ newClass basicNew ].

	newClass allSlots do: [ :newSlot | oldObject class slotNamed: newSlot name ifFound: [ :oldSlot | 
			newSlot write: (oldSlot read: oldObject) to: newObject ] ].

	newClass isVariable
		ifTrue: [ 1 to: oldObject basicSize do: [ :offset | newObject basicAt: offset put: (oldObject basicAt: offset) ] ].

	^ newObject