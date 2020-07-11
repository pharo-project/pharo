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
RBVariableNode >> isArg [
	self 
		deprecated: 'Use #isArgument instead.' 
		transformWith: '`@receiver isArg' -> '`@receiver isArgumentVariable'.
	
	^ self isArgument
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isArgOrTemp [
	^self binding isTemp or: [ self binding isArgumentVariable ]
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isClean [
	^ (self isInstance | self isReservedVariable) not
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isGlobal [
	"the semantics here is bad: all Literal Variables are Globals on the AST Level"
	^variable isLiteralVariable and: [ variable isUndeclared not]
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isInstance [
	^self binding isInstance
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isReservedVariable [
	^ self binding isReservedVariable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isUndeclared [

	^self binding isUndeclared
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable [
	^variable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> variable: aSemVar [
	variable := aSemVar
]
