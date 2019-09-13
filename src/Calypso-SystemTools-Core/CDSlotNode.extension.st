Extension { #name : #CDSlotNode }

{ #category : #'*Calypso-SystemTools-Core' }
CDSlotNode >> asCalypsoVariableOf: aClass [ 
	
	| actualClass |
	actualClass := self classDefinitionNode existingClassIfAbsent: [
		self error: 'Class is not exists yet'].
	^actualClass 
		slotNamed: name asSymbol 
		ifFound: [:aSlot | 
			^ClyInstanceVariable on: aSlot definedIn: actualClass] 
		ifNone: [self error: 'Slot is not created yet']
]
