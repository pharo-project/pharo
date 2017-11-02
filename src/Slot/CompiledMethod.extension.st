Extension { #name : #CompiledMethod }

{ #category : #'*Slot' }
CompiledMethod >> hasTemporaryVariableNamed: aName [
	^(self tempNames includes: aName)
	
]

{ #category : #'*Slot' }
CompiledMethod >> removeSavedTemp: aTemp [
	self savedTemps ifNotNil: [ :saved | 
			saved remove: aTemp.
			saved ifEmpty: [ self removeProperty: #savedTemps ] ]
]

{ #category : #'*Slot' }
CompiledMethod >> saveTemp: aTemp [
	self savedTemps ifNil: [ self propertyAt: #savedTemps put: Set new].
	self savedTemps add: aTemp.
]

{ #category : #'*Slot' }
CompiledMethod >> savedTemps [
	^self propertyAt: #savedTemps ifAbsent: nil
]

{ #category : #'*Slot' }
CompiledMethod >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		method: self 


]

{ #category : #'*Slot' }
CompiledMethod >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
