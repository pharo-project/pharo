"
I am a simple cache for AST nodes corresponding to CompiledMethods in the image. The cache is emptied when the image is saved.

The cached #ast is for one interesting for speed (that is, in situations where you ask for it often).

The other use-case is if you want to annotate the AST and keep that annotation around (till the next image save, but you can subscribe to ASTCacheReset and re-install the AST in the cache after cleaning. (This is used by MetaLinks to make sure they survive image restart).

The last thing that it provides is that we do have a quite powerful mapping between bytecode/text/context and the AST. Regardless how you navigate, you get the same object.

e.g. even this one works:

    [ 1+2 ] sourceNode == thisContext method ast blockNodes first

**NOTE** due to the cached ast, Modification of the AST can be a problem.
Code that wants to modify the AST without making sure the compiledMethod is in sync later should use #parseTree. 

"
Class {
	#name : #ASTCache,
	#superclass : #WeakIdentityKeyDictionary,
	#classInstVars : [
		'default'
	],
	#category : #'AST-Core-Parser'
}

{ #category : #accessing }
ASTCache class >> at: aCompiledMethod [
	^ default at: aCompiledMethod
]

{ #category : #accessing }
ASTCache class >> default [
	^ default
]

{ #category : #accessing }
ASTCache class >> default: anASTCache [
	default := anASTCache.
]

{ #category : #'class initialization' }
ASTCache class >> initialize [
	default := self new.
	SessionManager default registerSystemClassNamed: self name
]

{ #category : #'initialization-release' }
ASTCache class >> reset [
	default reset.
	SystemAnnouncer uniqueInstance announce: ASTCacheReset new 
]

{ #category : #'system startup' }
ASTCache class >> shutDown [	
	self reset
]

{ #category : #accessing }
ASTCache >> at: aCompiledMethod [
	^ self 
		at: aCompiledMethod
		ifAbsentPut: [ 
			aCompiledMethod parseTree doSemanticAnalysisIn: aCompiledMethod methodClass ]
]

{ #category : #'initialization-release' }
ASTCache >> reset [
	self removeAll
]
