Extension { #name : 'OCVariableNode' }

{ #category : '*OpalCompiler-Core' }
OCVariableNode >> binding [
	^self variable
]

{ #category : '*OpalCompiler-Core' }
OCVariableNode >> binding: aSemVar [
	self variable: aSemVar
]

{ #category : '*OpalCompiler-Core' }
OCVariableNode >> isAccessingSelf [
	^ self isInstanceVariable or: [ self isSelfVariable or: [self isSuperVariable]]
]

{ #category : '*OpalCompiler-Core' }
OCVariableNode >> variable [
	^variable
]

{ #category : '*OpalCompiler-Core' }
OCVariableNode >> variable: aSemVar [
	variable := aSemVar
]
