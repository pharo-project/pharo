Extension { #name : #TraitDescription }

{ #category : #'*Reflectivity' }
TraitDescription >> assignmentNodes [
	^self methods flatCollect: [ :each | each assignmentNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> blockNodes [
	^self methods flatCollect: [ :each | each blockNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> instanceVariableReadNodes [
	^self methods flatCollect: [ :each | each instanceVariableReadNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> instanceVariableWriteNodes [
	^self methods flatCollect: [ :each | each instanceVariableWriteNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> sendNodes [
	^self methods flatCollect: [ :each | each sendNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> variableNodes [
	^self methods flatCollect: [ :each | each variableNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> variableReadNodes [
	^self methods flatCollect: [ :each | each variableReadNodes ]
]

{ #category : #'*Reflectivity' }
TraitDescription >> variableWriteNodes [
	^self methods flatCollect: [ :each | each variableWriteNodes ]
]
