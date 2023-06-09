"
The ASTCache is queried when we request the ast (via #ast) of a CompiledMethod.
If the AST has already been requested before, the cache will return it.

To create the AST for a method the first time, the cache uses a strategy pattern. 
This class is the simple strategy: call #parseTree, so semantic analysis. 

If Reflectivity is loaded, the RFReflectivityASTCacheStrategy is used instead which
will take the AST from the ReflectiveMethod, should it have been created.
"
Class {
	#name : #ASTCacheMissStrategy,
	#superclass : #Object,
	#category : #'AST-Core-Parser'
}

{ #category : #accessing }
ASTCacheMissStrategy >> getASTFor: aCompiledMethod [
	^ aCompiledMethod parseTree
]
