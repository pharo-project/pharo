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
		'weakDictionary'
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

	^ self
		  at: aCompiledMethod
		  ifAbsentPut: [ self getASTFor: aCompiledMethod ]
]

{ #category : #accessing }
ASTCache >> at: aCompiledMethod ifAbsentPut: aBlock [
	"Get an AST using strongly held information, or failback to aBlock (that might compute a new AST)"

	"For doit methods, the AST is stored in the method property"
	(aCompiledMethod propertyAt: #ast) ifNotNil: [ :ast | ^ ast ].

	"Reflective methods have a strongly held AST, return this one"
	(aCompiledMethod propertyAt: #reflectiveMethod) ifNotNil: [ :rf | ^ rf ast ].

	"Look in the (almost infinite) cache"
	self weakDictionary at: aCompiledMethod ifPresent: [ :ast | ^ ast ].

	"We tried everything we could. So compute and store it"
	^ self at: aCompiledMethod put: aBlock value
]

{ #category : #accessing }
ASTCache >> at: aCompiledMethod put: aRBMethodNode [

	^ self weakDictionary at: aCompiledMethod put: aRBMethodNode
]

{ #category : #accessing }
ASTCache >> getASTFor: aCompiledMethod [

	^ self class cacheMissStrategy getASTFor: aCompiledMethod
]

{ #category : #copying }
ASTCache >> postCopy [

	weakDictionary := weakDictionary copy
]

{ #category : #printing }
ASTCache >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: '#';
		print: self weakDictionary size
]

{ #category : #initialization }
ASTCache >> reset [
	self weakDictionary removeAll.
	weakDictionary := nil.
]

{ #category : #accessing }
ASTCache >> weakDictionary [

	^ weakDictionary ifNil: [
		  weakDictionary := WeakIdentityKeyDictionary new ]
]
