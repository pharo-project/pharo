Extension { #name : 'ASTVariableNode' }

{ #category : '*OpalCompiler-Core' }
ASTVariableNode >> binding [
	^self variable
]

{ #category : '*OpalCompiler-Core' }
ASTVariableNode >> binding: aSemVar [
	self variable: aSemVar
]

{ #category : '*OpalCompiler-Core' }
ASTVariableNode >> isAccessingSelf [
	^ self isInstanceVariable or: [ self isSelfVariable or: [self isSuperVariable]]
]

{ #category : '*OpalCompiler-Core' }
ASTVariableNode >> variable [
	^variable
]

{ #category : '*OpalCompiler-Core' }
ASTVariableNode >> variable: aSemVar [
	variable := aSemVar
]
