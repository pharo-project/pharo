"
A TRBProgramNodeVisitor is a simple Trait that define visitor methods. 
"
Trait {
	#name : #TRBProgramNodeVisitor,
	#category : #'AST-Core-Traits'
}

{ #category : #'accessing - method dictionary' }
TRBProgramNodeVisitor classSide >> addAndClassifySelector: arg1 withMethod: arg2 inProtocol: arg3 [

	self localMethodDict at: arg1 put: arg2.
	super addAndClassifySelector: arg1 withMethod: arg2 inProtocol: arg3.
	TraitChange addSelector: arg1 on: self
]

{ #category : #'accessing - method dictionary' }
TRBProgramNodeVisitor classSide >> addSelector: arg1 withMethod: arg2 [

	self localMethodDict at: arg1 put: arg2.
	super addSelector: arg1 withMethod: arg2.
	TraitChange addSelector: arg1 on: self
]

{ #category : #'accessing - method dictionary' }
TRBProgramNodeVisitor classSide >> addSelector: arg1 withRecompiledMethod: arg2 [

	arg2 isFromTrait ifTrue: [
		^ super addSelector: arg1 withRecompiledMethod: arg2 ].
	self localMethodDict at: arg1 put: arg2.
	super addSelector: arg1 withRecompiledMethod: arg2.
	TraitChange addSelector: arg1 on: self
]

{ #category : #querying }
TRBProgramNodeVisitor classSide >> allTraits [

	^ self traitComposition allTraits
]

{ #category : #initialization }
TRBProgramNodeVisitor classSide >> doRebuildMethodDictionary [

	| tmp1 tmp2 tmp3 tmp5 |
	tmp5 := false.
	self methodDict valuesDo: [ :arg1 |
		| tmp7 |
		tmp7 := arg1 traitSource.
		tmp7
			ifNil: [ self localMethodDict at: arg1 selector put: arg1 ]
			ifNotNil: [ tmp7 ] ].
	tmp1 := self traitComposition selectors reject: [ :arg2 |
		        self localMethodDict includesKey: arg2 ].
	tmp1 do: [ :arg3 |
		tmp5 := tmp5 | (self traitComposition
			         installSelector: arg3
			         into: self
			         replacing: false) ].
	tmp2 := self methodDict keys reject: [ :arg4 |
		        (tmp1 includes: arg4) or: [
			        self localMethodDict includesKey: arg4 ] ].
	tmp5 := tmp5 | tmp2 isNotEmpty.
	tmp2 do: [ :arg5 | self methodDict removeKey: arg5 ].
	tmp3 := self organization allMethodSelectors reject: [ :arg6 |
		        self methodDict includesKey: arg6 ].
	tmp3 do: [ :arg7 | self organization removeElement: arg7 ].
	^ tmp5
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> findOriginClassOf: arg1 [

	(arg1 hasProperty: #traitSource) ifTrue: [
		^ arg1 traitSource innerClass ].
	(self includesLocalSelector: arg1 selector) ifTrue: [ ^ self ].
	^ (self traitComposition
		   traitDefining: arg1 selector
		   ifNone: [ ^ self ]) innerClass
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> findOriginMethodOf: arg1 [

	| tmp1 |
	(self includesLocalSelector: arg1 selector) ifTrue: [ ^ arg1 ].
	(arg1 hasProperty: #traitSource) ifTrue: [
		tmp1 := self traitComposition originSelectorOf: arg1 selector.
		^ arg1 traitSource compiledMethodAt: tmp1 ifAbsent: [ arg1 ] ].
	^ (self traitComposition
		   traitDefining: arg1 selector
		   ifNone: [ self ])
		  compiledMethodAt: arg1 selector
		  ifAbsent: [ ^ arg1 ]
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> hasTraitComposition [

	^ self traitComposition isEmpty not
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> includesLocalSelector: arg1 [

	^ self isLocalSelector: arg1
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> isAliasSelector: arg1 [

	^ self traitComposition isAliasSelector: arg1
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> isLocalAliasSelector: arg1 [

	^ self traitComposition isLocalAliasSelector: arg1
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> isLocalMethodsProtocol: arg1 [

	arg1 methodSelectors ifEmpty: [ ^ true ].
	^ arg1 methodSelectors anySatisfy: [ :arg2 |
		  self isLocalSelector: arg2 ]
]

{ #category : #testing }
TRBProgramNodeVisitor classSide >> isLocalSelector: arg1 [

	^ self localMethodDict includesKey: arg1
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> localMethodDict [

	^ self class baseLocalMethods
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> localMethodDict: arg1 [

	^ self class baseLocalMethods: arg1
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> localMethods [

	^ self localMethodDict values
]

{ #category : #'accessing - method dictionary' }
TRBProgramNodeVisitor classSide >> localSelectors [

	^ self localMethodDict keys
]

{ #category : #categories }
TRBProgramNodeVisitor classSide >> recategorizeSelector: arg1 from: arg2 to: arg3 [

	| tmp1 |
	tmp1 := self organization
		        categoryOfElement: arg1
		        ifAbsent: [ ^ self ].
	arg3 ifNil: [ ^ self ].
	tmp1 = arg2 ifTrue: [
		self organization classify: arg1 under: arg3 suppressIfDefault: true ].
	(self traitComposition reverseAlias: arg1) do: [ :arg4 |
		self
			recategorizeSelector: arg4 from: arg2 to: arg3;
			notifyOfRecategorizedSelector: arg4 from: arg2 to: arg3 ].
	self organization removeEmptyCategories
]

{ #category : #recompilation }
TRBProgramNodeVisitor classSide >> recompile: arg1 from: arg2 [

	| tmp1 tmp2 tmp4 |
	tmp1 := arg2 compiledMethodAt: arg1.
	tmp4 := self compiler.
	tmp4
		source: (arg2 sourceCodeAt: arg1);
		class: self;
		failBlock: [ ^ self ];
		compiledMethodTrailer: tmp1 trailer.
	tmp2 := tmp4 compile.
	arg1 == tmp2 selector ifFalse: [ self error: 'selector changed!' ].
	tmp1 properties
		at: #traitSource
		ifPresent: [ :arg3 | tmp2 propertyAt: #traitSource put: arg3 ].
	self addSelector: arg1 withRecompiledMethod: tmp2
]

{ #category : #'trait-composition' }
TRBProgramNodeVisitor classSide >> removeFromComposition: arg1 [

	self setTraitComposition:
		(self traitComposition copyWithoutTrait: arg1 asTraitComposition)
]

{ #category : #removing }
TRBProgramNodeVisitor classSide >> removeSelector: arg1 [

	super removeSelector: arg1.
	self localMethodDict removeKey: arg1 ifAbsent: [  ].
	TraitChange removeSelector: arg1 on: self
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> traitComposition [

	^ self class baseComposition
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> traitComposition: arg1 [

	arg1 asTraitComposition allTraits do: [ :arg2 |
		arg2 isTrait ifFalse: [
			self error:
				'All the members of the trait composition should be traits' ] ].
	self class baseComposition: arg1
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> traitCompositionString [

	^ self traitComposition asString
]

{ #category : #accessing }
TRBProgramNodeVisitor classSide >> traits [

	^ self traitComposition traits
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [
	"Sent *once* when visiting method and block nodes"
	^aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArgumentVariableNode: anArgumentNode [

	^ self visitLocalVariableNode: anArgumentNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArrayNode: anArrayNode [

	anArrayNode children do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [

	self
		visitNode: anAssignmentNode variable;
		visitNode: anAssignmentNode value
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitBlockNode: aBlockNode [
	self
		visitArgumentNodes: aBlockNode arguments;
		visitNode: aBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [:aMessage | self visitNode: aMessage]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitClassVariableNode: aNode [
	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitEnglobingErrorNode: anEnglobingErrorNode [
	anEnglobingErrorNode content do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitGlobalVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralValueNode: aNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralVariableNode: aNode [
	"to be backward compatible, we visit for Gloabls here (there used to be no difference)"

	^ self visitGlobalNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLocalVariableNode: aNode [
	"call visitTemporaryNode: for backward compatibility"
	^ self visitTemporaryNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitMessageNode: aMessageNode [
	(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded])
		ifTrue: [self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitMethodNode: aMethodNode [
	self visitArgumentNodes: aMethodNode arguments.
	aMethodNode pragmas
		do: [:each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitNode: aNode [
	^aNode acceptVisitor: self
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [
	self
		visitArgumentNodes: aRBPatternBlockNode arguments;
		visitNode: aRBPatternBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self
		visitNode: aRBPatternWrapperBlockNode wrappedNode;
		visitArgumentNodes: aRBPatternWrapperBlockNode arguments;
		visitNode: aRBPatternWrapperBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitReturnNode: aReturnNode [
	^ self visitNode: aReturnNode value
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSequenceNode: aSequenceNode [
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]
]

{ #category : #'visiting - reflectivity' }
TRBProgramNodeVisitor >> visitStoreIntoTempNode: aNode [
	^ aNode
]

{ #category : #'visiting - reflectivity' }
TRBProgramNodeVisitor >> visitStorePopIntoTempNode: aNode [
	^ aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	IT redirects the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryNode: aNode [
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryVariableNode: aNode [

	^ self visitLocalVariableNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitUnreachableStatement: anUnreachableStatement [

	^ self visitEnglobingErrorNode: anUnreachableStatement
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
