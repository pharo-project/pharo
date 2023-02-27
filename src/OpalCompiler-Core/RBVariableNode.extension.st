Extension { #name : #RBVariableNode }

{ #category : #'*OpalCompiler-Core' }
RBVariableNode >> binding [
	^self variable
]

{ #category : #'*OpalCompiler-Core' }
RBVariableNode >> binding: aSemVar [
	self variable: aSemVar
]

{ #category : #'*OpalCompiler-Core' }
RBVariableNode >> isClean [
	^ (self isInstanceVariable | self isSelfVariable | self isSuperVariable) not
]

{ #category : #'*OpalCompiler-Core' }
RBVariableNode >> variable [
	^variable
]

{ #category : #'*OpalCompiler-Core' }
RBVariableNode >> variable: aSemVar [
	variable := aSemVar
]
