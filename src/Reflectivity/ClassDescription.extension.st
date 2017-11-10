Extension { #name : #ClassDescription }

{ #category : #'*Reflectivity' }
ClassDescription >> assignmentNodes [
	^self methods flatCollect: [ :each | each assignmentNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> blockNodes [
	^self methods flatCollect: [ :each | each blockNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> instanceVariableReadNodes [
	^self methods flatCollect: [ :each | each instanceVariableReadNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> instanceVariableWriteNodes [
	^self methods flatCollect: [ :each | each instanceVariableWriteNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> sendNodes [
	^self methods flatCollect: [ :each | each sendNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> variableNodes [
	^self methods flatCollect: [ :each | each variableNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> variableReadNodes [
	^self methods flatCollect: [ :each | each variableReadNodes ]
]

{ #category : #'*Reflectivity' }
ClassDescription >> variableWriteNodes [
	^self methods flatCollect: [ :each | each variableWriteNodes ]
]
