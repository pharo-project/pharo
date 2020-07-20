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
	^ (self isInstanceVariable | self isReservedVariable) not
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isGlobal [
	"the semantics here is bad: all Literal Variables are Globals on the AST Level"
	^variable isLiteralVariable and: [ variable isUndeclaredVariable not]
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable [
	^variable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable: aSemVar [
	variable := aSemVar
]
