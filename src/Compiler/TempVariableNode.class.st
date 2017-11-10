"
I am a parse tree leaf representing a temporary variable
"
Class {
	#name : #TempVariableNode,
	#superclass : #VariableNode,
	#instVars : [
		'argType',
		'hasRefs',
		'hasDefs',
		'scope',
		'definingScope',
		'readingScopes',
		'writingScopes',
		'remoteNode'
	],
	#category : #Compiler-ParseNodes
}

{ #category : #visiting }
TempVariableNode >> accept: aVisitor [
	^aVisitor visitTempVariableNode: self
]

{ #category : #'code generation (closures)' }
TempVariableNode >> addReadWithin: scopeBlock "<BlockNode>" at: location [ "<Integer>"
	readingScopes ifNil: [readingScopes := Dictionary new].
	(readingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
]

{ #category : #'code generation (closures)' }
TempVariableNode >> addWriteWithin: scopeBlock "<BlockNode>" at: location [ "<Integer>"
	writingScopes ifNil: [writingScopes := Dictionary new].
	(writingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
]

{ #category : #'code generation (closures)' }
TempVariableNode >> analyseClosure: rootNode [ "<MethodNode>"
	"Analyse whether the temporary needs to be made remote
	 or not, and answer whether it was made remote.
	 A temp cannot be local if it is written to remotely,
	 or if it is written to after it is closed-over.  An exception
	 is an inlined block argument that appears to be written
	 remotely but is actually local to a block."
	| latestWrite |
	self isBlockArg ifTrue: [^false].
	remoteNode ifNotNil: [^false]. "If already remote, don't remote a second time"
	latestWrite := 0.
	((writingScopes notNil
	 and: [writingScopes associations anySatisfy: [:assoc|
			[:blockScope :refs|
			refs do: [:write| latestWrite := write max: latestWrite].
			"A temp cannot be local if it is written to remotely."
			blockScope actualScope ~~ definingScope actualScope]
				value: assoc key value: assoc value]])
	or: [readingScopes notNil
		and: [readingScopes associations anySatisfy: [:assoc|
				[:blockScope :refs|
				 "A temp cannot be local if it is written to after it is closed-over."
				 blockScope actualScope ~~ definingScope actualScope
				 and: [refs anySatisfy: [:read| read < latestWrite]]]
					value: assoc key value: assoc value]]]) ifTrue:
		[remoteNode := definingScope addRemoteTemp: self rootNode: rootNode.
		 ^true].
	^false
]

{ #category : #'code generation (closures)' }
TempVariableNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	self addReadWithin: scopeBlock at: rootNode locationCounter
]

{ #category : #testing }
TempVariableNode >> assignmentCheck: encoder at: location [
	^(self isBlockArg or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]
]

{ #category : #testing }
TempVariableNode >> beBlockArg [
	argType := #block
]

{ #category : #testing }
TempVariableNode >> beMethodArg [
	argType := #method
]

{ #category : #'code generation (closures)' }
TempVariableNode >> beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	self addWriteWithin: scopeBlock at: rootNode locationCounter.
	"For analysis of optimized blocks also record the set of temporaries written to
	 within optimized blocks so that additional writes can be added at locations that
	 represent subsequent iterations of the loop. e.g. testInlineBlockCollectionSD1"
	assignmentPools keysAndValuesDo:
		[:outerScopeBlock :set|
		"definingScope can be nil in expr in expr ifNil: [:arg|...] expressions because
		 arg gets its definingScope set when [:arg|...] is analysed."
		outerScopeBlock actualScope
			= (definingScope
				ifNil: [scopeBlock]
				ifNotNil: [definingScope actualScope]) ifTrue:
			[set add: self]]
]

{ #category : #'code generation (closures)' }
TempVariableNode >> definingScope [
	^definingScope
]

{ #category : #'code generation (closures)' }
TempVariableNode >> definingScope: scopeBlock [ 
	"<BlockNode>"
	definingScope ifNotNil: [self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock
]

{ #category : #'code generation' }
TempVariableNode >> emitCodeForLoad: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[remoteNode emitCodeForLoadFor: self stack: stack encoder: encoder]
]

{ #category : #'code generation' }
TempVariableNode >> emitCodeForStore: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStoreInto: self stack: stack encoder: encoder].
	encoder genStoreTemp: index
]

{ #category : #'code generation' }
TempVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStorePopInto: self stack: stack encoder: encoder].
	encoder genStorePopTemp: index.
	stack pop: 1
]

{ #category : #'code generation' }
TempVariableNode >> emitCodeForValue: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForValueOf: self stack: stack encoder: encoder].
	encoder genPushTemp: index.
	stack push: 1
]

{ #category : #'code generation (closures)' }
TempVariableNode >> index: anInteger [
	"For renumbering temps in the closure compiler."
	index := anInteger.
	code := self code: index type: LdTempType
]

{ #category : #testing }
TempVariableNode >> isArg [
	^argType notNil
]

{ #category : #testing }
TempVariableNode >> isBlockArg [
	^#block == argType
]

{ #category : #testing }
TempVariableNode >> isDeclaredAtMethodLevel [
	"For the explainer."
	^scope = 0
]

{ #category : #'code generation (closures)' }
TempVariableNode >> isDefinedWithinBlockExtent: anInterval [
	^anInterval rangeIncludes: definingScope actualScope blockExtent first
]

{ #category : #'code generation (closures)' }
TempVariableNode >> isIndirectTempVector [
	^false
]

{ #category : #testing }
TempVariableNode >> isMethodArg [
	^#method == argType
]

{ #category : #'code generation (closures)' }
TempVariableNode >> isReferencedWithinBlockExtent: anInterval [ 
	readingScopes ~~ nil ifTrue:
		[readingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	^false
]

{ #category : #testing }
TempVariableNode >> isRemote [
	^remoteNode notNil
]

{ #category : #testing }
TempVariableNode >> isTemp [
	^ true
]

{ #category : #testing }
TempVariableNode >> isUndefTemp [
	^ hasDefs not
]

{ #category : #testing }
TempVariableNode >> isUnusedTemp [
	^ hasRefs not
]

{ #category : #initialize-release }
TempVariableNode >> name: varName index: i type: type scope: level [
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type
]

{ #category : #initialize-release }
TempVariableNode >> nowHasDef [
	hasDefs := true
]

{ #category : #initialize-release }
TempVariableNode >> nowHasRef [
	hasRefs := true
]

{ #category : #'code generation (closures)' }
TempVariableNode >> referenceScopesAndIndicesDo: aBinaryBlock [
	"Evaluate aBinaryBlock with all read or write scopes and locations.
	 This is used to copy the reference information into RemoteTempVectorNodes"
	readingScopes ~~ nil ifTrue:
		[readingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]]
]

{ #category : #testing }
TempVariableNode >> remoteNode [
	^remoteNode
]

{ #category : #decompiler }
TempVariableNode >> remoteNode: aRemoteTempVectorNode [
	remoteNode := aRemoteTempVectorNode
]

{ #category : #testing }
TempVariableNode >> scope [
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	^scope
]

{ #category : #initialize-release }
TempVariableNode >> scope: level [
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level
]

{ #category : #'code generation' }
TempVariableNode >> sizeCodeForLoad: encoder [
	^remoteNode
		ifNil: [0]
		ifNotNil: [remoteNode sizeCodeForLoadFor: self encoder: encoder]
]

{ #category : #'code generation' }
TempVariableNode >> sizeCodeForStore: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStoreInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStoreTemp: index
]

{ #category : #'code generation' }
TempVariableNode >> sizeCodeForStorePop: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStorePopInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStorePopTemp: index
]

{ #category : #'code generation' }
TempVariableNode >> sizeCodeForValue: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForValueOf: self encoder: encoder].
	self reserve: encoder.
	^encoder sizePushTemp: index
]
