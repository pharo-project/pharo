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
Variable >> asDoItVariableFrom: aContext [
	"Specific kind of variables may require special logic to be visible in DoIt expressions.
	For example local variables can't be accessed directly in DoIt expressions in the debugger:
		- DoIt is executed using new process where there are no temps from the original context.
	In such cases subclasses should return adapter variable here to represent receiver in DoIt method.
	DoItVariable class is implemented with this purpose (see the class comment and tests)"
	^ self
]

{ #category : '*OpalCompiler-Core' }
Variable >> isShadowing [
	"I shadow a variable if looking up my name in the outer scope returns a variable"
	^(self scope outerScope ifNotNil: [:outer | outer lookupVar: self name]) isNotNil
]
