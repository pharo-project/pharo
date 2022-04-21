Extension { #name : #ASTVariableNode }

{ #category : #'*OpalCompiler-Core' }
ASTVariableNode >> binding [
	^self variable
]

{ #category : #'*OpalCompiler-Core' }
ASTVariableNode >> binding: aSemVar [
	self variable: aSemVar
]

{ #category : #'*OpalCompiler-Core' }
ASTVariableNode >> isClean [
	^ (self isInstanceVariable | self isSelfVariable | self isSuperVariable) not
]

{ #category : #'*OpalCompiler-Core' }
ASTVariableNode >> variable [
	^variable
]

{ #category : #'*OpalCompiler-Core' }
ASTVariableNode >> variable: aSemVar [
	variable := aSemVar
]
