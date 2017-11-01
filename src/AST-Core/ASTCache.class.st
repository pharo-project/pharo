"
I am a simple cache for AST nodes corresponding to CompiledMethods in the image. The cache is emptied when the image is saved.
"
Class {
	#name : #ASTCache,
	#superclass : #WeakIdentityKeyDictionary,
	#classInstVars : [
		'default'
	],
	#category : 'AST-Core-Parser'
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

{ #category : #accessing }
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

{ #category : #accessing }
ASTCache >> reset [
	self removeAll
]
