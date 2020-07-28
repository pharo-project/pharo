"
The ASTCache is queried when we request the ast (via #ast) of a CompiledMethod.
If the AST has already been requested before, the cache will return it.

To create the AST for a method initially, the cache uses a strategy pattern. 

This RF strategy will take the AST from the ReflectiveMethod, should it have been created (due to MetaLinks installed on the AST).

The side effect is that the AST of these methods will survive a ASTCache reset and thus image restart.
"
Class {
	#name : #RFReflectivityASTCacheStrategy,
	#superclass : #Object,
	#category : #'Reflectivity-Compiler'
}

{ #category : #accessing }
RFReflectivityASTCacheStrategy >> getASTFor: aCompiledMethod [
	^ aCompiledMethod reflectiveMethod
		ifNil: [ aCompiledMethod parseTree
				doSemanticAnalysisIn: aCompiledMethod methodClass ]
		ifNotNil: [ :rfMethod | rfMethod ast ]
]
