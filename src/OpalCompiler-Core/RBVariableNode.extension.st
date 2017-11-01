Extension { #name : #RBVariableNode }

{ #category : #'*opalcompiler-core' }
RBVariableNode >> binding [
	^self propertyAt: #binding ifAbsent: [nil].
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> binding: aSemVar [
	aSemVar ifNil: [^self removeProperty: #binding ifAbsent: []].
	self propertyAt: #binding put: aSemVar.
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isArg [

	^self binding isArg
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isClean [
	^ (self isInstance | self isSpecialVariable) not
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isGlobal [
	^self binding isGlobal
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isInstance [
	^self binding isInstance
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isSpecialVariable [
	^ self binding isSpecialVariable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isTemp [
	^self binding isTemp
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isUndeclared [

	^self binding isUndeclared
]
