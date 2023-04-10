"
I am a simple cache for AST nodes corresponding to CompiledMethods in the image. The cache is emptied when the image is saved.

The cached #ast is for one interesting for speed (that is, in situations where you ask for it often).

The other use-case is if you want to annotate the AST and keep that annotation around (till the next image save, but you can subscribe to ASTCacheReset and re-install the AST in the cache after cleaning. (This is used by MetaLinks to make sure they survive image restart).

The last thing that it provides is that we do have a quite powerful mapping between bytecode/text/context and the AST. Regardless of how you navigate, you get the same object.

e.g. even this one works:

    [ 1+2 ] sourceNode == thisContext method ast blockNodes first

**NOTE** due to the cached AST, Modification of the AST can be a problem.
Code that wants to modify the AST without making sure the compiledMethod is in sync later should use #parseTree. 

"
Class {
	#name : #ASTCache,
	#superclass : #Object,
	#instVars : [
		'cache',
		'weakdict'
	],
	#classVars : [
		'CacheMissStrategy'
	],
	#classInstVars : [
		'default'
	],
	#category : #'AST-Core-Parser'
}

{ #category : #'private - announcements' }
ASTCache class >> announceCacheReset [

	SystemAnnouncer uniqueInstance announce: ASTCacheReset new
]

{ #category : #accessing }
ASTCache class >> at: aCompiledMethod [
	^ self default at: aCompiledMethod
]

{ #category : #accessing }
ASTCache class >> cacheMissStrategy [
	^ CacheMissStrategy
		ifNil: [ CacheMissStrategy := ASTCacheMissStrategy new ]
]

{ #category : #accessing }
ASTCache class >> cacheMissStrategy: aCacheMissStrategy [
	^ CacheMissStrategy := aCacheMissStrategy
]

{ #category : #accessing }
ASTCache class >> default [
	^ default ifNil: [
		SessionManager default registerSystemClassNamed: self name.
		default := self new ]
]

{ #category : #accessing }
ASTCache class >> default: anASTCache [
	default := anASTCache
]

{ #category : #'class initialization' }
ASTCache class >> reset [
	<script>
	self default reset.
	self announceCacheReset
]

{ #category : #'system startup' }
ASTCache class >> shutDown [
	self reset
]

{ #category : #accessing }
ASTCache >> at: aCompiledMethod [
	"For doit methods, the AST is stored in the method property"

	(aCompiledMethod propertyAt: #reflectiveMethod) ifNotNil: [
		:reflectiveMethod | ^ reflectiveMethod ast ].

	^ aCompiledMethod propertyAt: #ast ifAbsent: [
		weakdict at: aCompiledMethod ifPresent: [ :ast | ^ ast ].
		  self cache
			  ifNil: [ self getASTFor: aCompiledMethod ]
			  ifNotNil: [
				  self cache
					  at: aCompiledMethod
					  ifAbsentPut: [
					  self getASTFor: aCompiledMethod ] ] ]
]

{ #category : #accessing }
ASTCache >> at: aCollection ifAbsentPut: aRBMethodNode [

	self weakdict at: aCollection ifAbsentPut: [ aRBMethodNode ].
	self cache at: aCollection ifAbsentPut: [ aRBMethodNode ]
]

{ #category : #accessing }
ASTCache >> at: aCollection put: aRBMethodNode [

	self weakdict at: aCollection put: aRBMethodNode.
	self cache at: aCollection put: aRBMethodNode
]

{ #category : #accessing }
ASTCache >> cache [

	^ cache ifNil: [ "I don't want to depend on System-Caching. So, no system caching, no cache :P"
		  Smalltalk globals at: #LRUCache ifPresent: [ :class |
			  cache := class new
				           beThreadSafe;
				           maximumWeight: 1; "Force 1-slot LRU"
				           keyIndex: IdentityDictionary new ] ]
]

{ #category : #accessing }
ASTCache >> getASTFor: aCompiledMethod [

	| ast |
	ast := self class cacheMissStrategy getASTFor: aCompiledMethod.
	"weakdict is bad and can become large, a cleaning method exists in as an extension method in calypso..."
	(weakdict respondsTo: #clyCleanGarbage) ifTrue: [
		weakdict clyCleanGarbage ].
	weakdict at: aCompiledMethod put: ast.
	cache ifNotNil: [ cache at: aCompiledMethod put: ast ].
	^ ast
]

{ #category : #printing }
ASTCache >> printOn: aStream [

	super printOn: aStream.
	self cache printElementsOn: aStream
]

{ #category : #initialization }
ASTCache >> reset [

	self cache removeAll.
	self weakdict removeAll.
	cache := nil
]

{ #category : #accessing }
ASTCache >> weakdict [

	^ weakdict ifNil: [ weakdict := WeakIdentityValueDictionary new ]
]
