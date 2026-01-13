Extension { #name : 'Variable' }

{ #category : '*OpalCompiler-Core' }
Variable >> analyzeRead: aVariableNode by: aSemanticAnalyzer [

	self isUninitialized ifTrue: [ aSemanticAnalyzer uninitializedVariable: aVariableNode ]
]

{ #category : '*OpalCompiler-Core' }
Variable >> analyzeWrite: aVariableNode by: aSemanticAnalyzer [

	self isWritable ifFalse: [ aSemanticAnalyzer storeIntoReadOnlyVariable: aVariableNode ]
]

{ #category : '*OpalCompiler-Core' }
Variable >> isShadowing [
	"I shadow a variable if looking up my name in the outer scope returns a variable"
	^(self scope outerScope ifNotNil: [:outer | outer lookupVar: self name]) isNotNil
]
