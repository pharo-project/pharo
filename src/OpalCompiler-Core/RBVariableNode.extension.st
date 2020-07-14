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
RBVariableNode >> isClean [
	^ (self isInstanceVariable | self isReservedVariable) not
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isGlobal [
	"the semantics here is bad: all Literal Variables are Globals on the AST Level"
	^variable isLiteralVariable and: [ variable isUndeclared not]
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isInstance [
	self 
		deprecated: 'Use #isInstanceVariable instead.' 
		transformWith: '`@receiver isInstance' -> '`@receiver isInstanceVariable'.
	^variable isInstanceVariable
]

{ #category : #'*opalcompiler-core' }
RBVariableNode >> isLocalVariable [
	"returns true for temporary variables and arguments"
	^variable isLocalVariable
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
