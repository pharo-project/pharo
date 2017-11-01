Extension { #name : #TClassDescription }

{ #category : #'*Reflectivity' }
TClassDescription >> assignmentNodes [
	^self methods flatCollect: [ :each | each assignmentNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> blockNodes [
	^self methods flatCollect: [ :each | each blockNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> instanceVariableReadNodes [
	^self methods flatCollect: [ :each | each instanceVariableReadNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> instanceVariableWriteNodes [
	^self methods flatCollect: [ :each | each instanceVariableWriteNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> sendNodes [
	^self methods flatCollect: [ :each | each sendNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> variableNodes [
	^self methods flatCollect: [ :each | each variableNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> variableReadNodes [
	^self methods flatCollect: [ :each | each variableReadNodes ]
]

{ #category : #'*Reflectivity' }
TClassDescription >> variableWriteNodes [
	^self methods flatCollect: [ :each | each variableWriteNodes ]
]
