Extension { #name : #CompiledMethod }

{ #category : #'*Slot-Core' }
CompiledMethod >> hasTemporaryVariableNamed: aName [
	^ self tempNames includes: aName
]

{ #category : #'*Slot-Core' }
CompiledMethod >> removeSavedTemp: aTemp [
	self savedTemps ifNotNil: [ :saved | 
			saved remove: aTemp.
			saved ifEmpty: [ self removeProperty: #savedTemps ] ]
]

{ #category : #'*Slot-Core' }
CompiledMethod >> saveTemp: aTemp [
	self savedTemps ifNil: [ self propertyAt: #savedTemps put: Set new].
	self savedTemps add: aTemp.
]

{ #category : #'*Slot-Core' }
CompiledMethod >> savedTemps [
	^self propertyAt: #savedTemps ifAbsent: nil
]

{ #category : #'*Slot-Core' }
CompiledMethod >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		method: self 


]

{ #category : #'*Slot-Core' }
CompiledMethod >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
