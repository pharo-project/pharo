Extension { #name : #CompiledMethod }

{ #category : #'*Slot-Core' }
CompiledMethod >> hasTemporaryVariableNamed: aName [
	^ self tempNames includes: aName
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
	^self tempNames collect: [ :name | TemporaryVariable name: name method: self]

]
