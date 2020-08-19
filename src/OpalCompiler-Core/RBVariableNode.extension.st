Extension { #name : #RBVariableNode }

{ #category : #'*opalcompiler-core' }
RBVariableNode >> binding [
	^variable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> binding: aSemVar [
	variable := aSemVar
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isClean [
	^ (self isInstanceVariable | self isSelfVariable | self isSuperVariable) not
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable [
	^variable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable: aSemVar [
	variable := aSemVar
]
