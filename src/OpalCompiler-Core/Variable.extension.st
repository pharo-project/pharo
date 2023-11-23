Extension { #name : 'Variable' }

{ #category : '*OpalCompiler-Core' }
Variable >> analyzeRead: aVariableNode by: aSemanticAnalyzer [

	self isUninitialized ifTrue: [ aSemanticAnalyzer uninitializedVariable: aVariableNode ]
]

{ #category : '*OpalCompiler-Core' }
Variable >> analyzeWrite: aVariableNode by: aSemanticAnalyzer [

	self isWritable ifFalse: [ aSemanticAnalyzer storeIntoReadOnlyVariable: aVariableNode ]
]
