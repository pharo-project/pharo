Class {
	#name : #Ring2ChunkImporter,
	#superclass : #Object,
	#instVars : [
		'environment',
		'package',
		'doIts'
	],
	#category : #'Ring2-ChunkImporter'
}

{ #category : #'instance creation' }
Ring2ChunkImporter class >> fromFileNamed: fileName [

	^ self new fromFileNamed: fileName; yourself
]

{ #category : #'instance creation' }
Ring2ChunkImporter class >> fromStream: aStream [

	^ self new fileInFrom: aStream; yourself
]

{ #category : #accessing }
Ring2ChunkImporter >> allowedSelectors [ 

	^ #(
#ephemeronSubclass:instanceVariableNames:classVariableNames:package:
#ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:

#immediateSubclass:instanceVariableNames:classVariableNames:package:
#immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#immediateSubclass:uses:instanceVariableNames:classVariableNames:package:
#immediateSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#subclass:
#subclass:instanceVariableNames:
#subclass:instanceVariableNames:classVariableNames:category:
#subclass:instanceVariableNames:classVariableNames:package:
#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#subclass:instanceVariableNames:classVariableNames:poolDictionaries:package:

#subclass:layout:slots:classVariables:category:
#subclass:layout:slots:classVariables:poolDictionaries:category:
#subclass:slots:classVariables:category:
#subclass:slots:classVariables:poolDictionaries:category:
#subclass:uses:
#subclass:uses:instanceVariableNames:classVariableNames:category:
#subclass:uses:instanceVariableNames:classVariableNames:package:
#subclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#subclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:
#subclass:uses:layout:slots:classVariables:category:
#subclass:uses:layout:slots:classVariables:poolDictionaries:category:
#subclass:uses:slots:classVariables:category:
#subclass:uses:slots:classVariables:poolDictionaries:category:

#variableByteSubclass:instanceVariableNames:classVariableNames:category:
#variableByteSubclass:instanceVariableNames:classVariableNames:package:
#variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableByteSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableByteSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:

#variableSubclass:instanceVariableNames:classVariableNames:category:
#variableSubclass:instanceVariableNames:classVariableNames:package:
#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#variableSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableSubclass:uses:instanceVariableNames:classVariableNames:package:
#variableSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#variableWordSubclass:instanceVariableNames:classVariableNames:category:
#variableWordSubclass:instanceVariableNames:classVariableNames:package:
#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:package:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#weakSubclass:instanceVariableNames:classVariableNames:category:
#weakSubclass:instanceVariableNames:classVariableNames:package:
#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#weakSubclass:uses:instanceVariableNames:classVariableNames:category:
#weakSubclass:uses:instanceVariableNames:classVariableNames:package:
#weakSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#weakSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#named:
#named:uses:category:
#named:uses:package:

instanceVariableNames:
#uses:
#uses:instanceVariableNames:
)
]

{ #category : #private }
Ring2ChunkImporter >> classDefinition: aString with: chgRec [
	| tokens theClass superclass |
	
	tokens := self scannedSource: aString.

	theClass := self classNamed: tokens third.
	theClass metaclass makeResolved.
	superclass := self classNamed: tokens first.
	superclass metaclass makeResolved.
	theClass superclass: superclass.
	theClass metaclass superclass: superclass metaclass.
	
	theClass cleanClassVariables.
	theClass cleanSharedPools.
	
	"theClass definitionSource: aString."
	
	theClass category: tokens last.
	
	tokens size = 11
	ifTrue:[ 
		theClass instanceVariables: (tokens fifth findTokens: ' ');
			 	classVariables: (tokens seventh findTokens: ' ');
				 sharedPools: (tokens ninth findTokens: ' ') ].
	
	tokens size = 13
	ifTrue:[
		RG2TraitCompositionVisitor new parse: (tokens fifth) for: theClass traitComposition.
		theClass 
			instanceVariables: (tokens seventh findTokens: ' ');
			classVariables: (tokens ninth findTokens: ' ');
			sharedPools: ((tokens at: 11) findTokens: ' '). ].
]

{ #category : #private }
Ring2ChunkImporter >> classNamed: className [

	| behavior |
	behavior := self environment ask behaviorNamed: className.
	^ behavior 
		ifNotNil: [ behavior ]
		ifNil: [ self environment ensureClassNamed: className asSymbol. ]

]

{ #category : #accessing }
Ring2ChunkImporter >> classes [
	^ package definedClasses 
]

{ #category : #private }
Ring2ChunkImporter >> createBehavior: subclassName superclassName: superclassName instanceVariableNames: instanceVariableNames classVariableNames: classVariableNames categoryName: categoryName packageName: packageName poolDictionariesNames: poolDictionariesNames layoutClass: layoutClass layoutDefinition: layoutDefinition slotsDefinition: slotsDefinition traits: traitsDefinition isTrait: isTrait hasNilSuperclass: hasNilSuperclass [

	| behavior superclass |
	
	self assert: subclassName notNil.	
		
	isTrait 
		ifTrue: [ 
			superclass := nil.
			behavior := (subclassName endsWith: ' classTrait') 
				ifTrue: [ 
					self environment ensureMetaclassTraitNamed: subclassName ]
				ifFalse: [ 
					self assert: (superclassName = #Trait).
					self environment ensureTraitNamed: subclassName ]]
		ifFalse: [ 
			behavior := self environment ensureClassNamed: subclassName. 
			hasNilSuperclass
				ifTrue: [ behavior superclass: nil. ]
				ifFalse: [
					superclassName ifNotNil: [ 
						| theSuperclass | 
						theSuperclass := self environment ensureClassNamed: superclassName. 
						behavior superclass: theSuperclass ] ] ].

	isTrait ifFalse: [ 
		layoutClass ifNotNil: [ 
			behavior layout: (layoutClass parent: behavior).
			"TODO: unknown layout definition"]].

	slotsDefinition ifNotNil: [ 
		behavior layout: (layoutClass parent: behavior).
		slotsDefinition children do: [:child |
			| newSlot | 
			newSlot := (child isLiteralNode and: [ child value isSymbol ])
				ifTrue: [ RG2InstanceVariableSlotDefinition named: child value parent: behavior layout ]
				ifFalse: [
					"TODO: probably needs more robust analysis"
					self assert: child isMessage.
					(RG2UnknownSlotDefinition named: (self slotNameFor: child) parent: behavior layout)
						expression: child formattedCode;
						yourself].
			behavior layout addSlot: newSlot] .].

	"process instance variables after layout and slots"
	instanceVariableNames ifNotNil: [ 
		behavior instanceVariables: (instanceVariableNames substrings collect: #asSymbol)].

	classVariableNames ifNotNil: [ 
		behavior classVariables: (classVariableNames substrings collect: #asSymbol) ].

	poolDictionariesNames ifNotNil: [ 
		behavior sharedPools: (poolDictionariesNames substrings collect: #asSymbol)].

	categoryName ifNotNil: [ 
		behavior category: categoryName ].

	packageName ifNotNil: [ 
		| pkg |
		pkg := environment ensurePackageNamed: packageName.
		behavior package: pkg ].

	traitsDefinition ifNotNil: [ 
		| composition transformations |
		composition := RG2TraitCompositionDefinition parent: behavior.
		transformations := RG2TraitCompositionVisitor new parse: traitsDefinition for: composition.
		behavior traitComposition: composition.
		transformations do: [ :each |
			composition addTransformation: each ].
		].
	
]

{ #category : #accessing }
Ring2ChunkImporter >> doIts [
	^ doIts
]

{ #category : #accessing }
Ring2ChunkImporter >> environment [

	^ environment
]

{ #category : #accessing }
Ring2ChunkImporter >> environment: aRG2Environment [

	environment := aRG2Environment
]

{ #category : #reading }
Ring2ChunkImporter >> fileInFrom: aStream [

	| changes |
	changes := (CodeImporter readStream: aStream) parseChunks.
	changes do: [ :change | change accept: self ].
]

{ #category : #reading }
Ring2ChunkImporter >> fromFileNamed: aName [
	
	package := RG2PackageDefinition named: aName.
	self fileInFrom: aName asFileReference readStream.
]

{ #category : #private }
Ring2ChunkImporter >> if: selectorParts in: ast includes: aSymbol do: aBlock [

	| index |
	
	index := selectorParts indexOf: aSymbol ifAbsent: [^ self].
	aBlock value: (ast arguments at: index).
]

{ #category : #initialization }
Ring2ChunkImporter >> initialize [
	super initialize.
	environment := RG2Environment new.
	package := environment ensurePackageNamed: 'ChangeSet'.
	doIts := Set new.
]

{ #category : #visitor }
Ring2ChunkImporter >> isNilSuperclassDefinition: ast [

	^ (ast isSequence) 
		and: [(ast statements size = 2)
		and: [(ast statements allSatisfy: #isMessage)
		and: [(self allowedSelectors includes: ast statements first selector)
		and: [(ast statements second selector = #superclass:)
		and: [(ast statements second arguments first value isNil)
		and: [((ast statements first arguments first value withoutPrefix: '#') = ast statements second receiver formattedCode)]]]]]]
		
]

{ #category : #private }
Ring2ChunkImporter >> metaClassDefinition: string with: chgRec [
	| tokens theClass |
	
	tokens := self scannedSource: string.
	
	theClass := self classNamed: tokens first.
	theClass theMetaClass instanceVariables: (tokens fourth findTokens: ' ')
]

{ #category : #private }
Ring2ChunkImporter >> metaTraitDefinition: string with: chgRec [
	| tokens trait |
	
	tokens := self scannedSource: string.
	trait := environment ensureTraitNamed: tokens first.
	
"	trait theMetaClass definitionSource: string"
]

{ #category : #private }
Ring2ChunkImporter >> msgClassComment: string with: chgRec [

	| tokens theClass |
	tokens := self scannedSource: string.
	
	(tokens size >= 3
		and:[ tokens last isString ])
	ifTrue:[
		theClass := self classNamed: tokens first.
		((tokens at: (tokens size - 1)) == #class
		ifTrue: [ theClass theMetaClass ]
		ifFalse:[ theClass ]) comment: tokens last asString ]
]

{ #category : #accessing }
Ring2ChunkImporter >> packages [
	
	^ { package }
]

{ #category : #private }
Ring2ChunkImporter >> scannedSource: aString [

	^ Smalltalk compiler parseLiterals: aString
]

{ #category : #private }
Ring2ChunkImporter >> slotNameFor: definitionAST [

	^ (definitionAST respondsTo: #receiver)
		ifTrue: [ self slotNameFor: definitionAST receiver  ]
		ifFalse: [ definitionAST value  ]
]

{ #category : #private }
Ring2ChunkImporter >> traitDefinition: aString with: chgRec [

	| tokens trait |
	tokens := self scannedSource: aString.
	trait := environment ensureTraitNamed: tokens third.
	"trait superclassName: tokens first;
			 definitionSource: aString;"
	trait category: tokens last
]

{ #category : #visitor }
Ring2ChunkImporter >> visitClassCommentChunk: aChunk [

	| class comment |
	
	class := self classNamed: aChunk behaviorName.
	comment := RG2CommentDefinition parent: class.
	comment content: aChunk contents.
	comment author: (comment authorForStamp: aChunk stamp).
	comment time: (comment timeForStamp: aChunk stamp).
	class comment: comment.
	
]

{ #category : #visitor }
Ring2ChunkImporter >> visitClassOrganizationChunk: aChunk [

	"Do nothing with class organizations for the moment"
]

{ #category : #visitor }
Ring2ChunkImporter >> visitDoItChunk: aChunk [

	| contents ast layoutClass selectorParts superclassName subclassName instanceVariableNames classVariableNames categoryName packageName poolDictionariesNames layoutDefinition slotsDefinition traitsDefinition isTrait hasNilSuperclass |

	contents := aChunk contents trimBoth.
	(contents endsWith: '!') ifTrue: [ contents := contents allButLast ].
	
	ast := RBParser parseExpression: contents onError: [ 
		doIts add: aChunk.
		^ self ].
	
	layoutClass := selectorParts := superclassName := subclassName := instanceVariableNames := 
		classVariableNames := categoryName := packageName := poolDictionariesNames := layoutDefinition := 
		slotsDefinition := traitsDefinition := nil.
		
	isTrait := false.
	
	hasNilSuperclass := self isNilSuperclassDefinition: ast.
	hasNilSuperclass 
		ifTrue: [ ast := ast statements first ].	
	
	(ast isMessage and: [self allowedSelectors includes: ast selector]) ifTrue: [ 
		superclassName := ast receiver formattedCode.
		self assert: ast arguments isNotEmpty.
		selectorParts := ast selector findBetweenSubstrings: {$:}.
		
		(superclassName endsWith: ' classTrait') 
			ifTrue: [isTrait := true]. "for usage of #uses:"
		
		"TODO: handle removeSelector:, comment:"

		self if: selectorParts in: ast includes: #subclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2FixedLayoutDefinition. ].

		self if: selectorParts in: ast includes: #named do: [:argument |
			subclassName := argument value. 
			isTrait := true.
			layoutClass := RG2FixedLayoutDefinition. ].

		self if: selectorParts in: ast includes: #immediateSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2ImmediateLayoutDefinition ].

		self if: selectorParts in: ast includes: #variableSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2VariableLayoutDefinition ].

		self if: selectorParts in: ast includes: #variableByteSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2ByteLayoutDefinition ].
		
		self if: selectorParts in: ast includes: #variableWordSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2WordLayoutDefinition ].

		self if: selectorParts in: ast includes: #weakSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2WeakLayoutDefinition ].
		
		self if: selectorParts in: ast includes: #ephemeronSubclass do: [:argument |
			subclassName := argument value. 
			layoutClass := RG2EphemeronLayoutDefinition ].
		
		self if: selectorParts in: ast includes: #instanceVariableNames do: [:argument |
			instanceVariableNames := argument value. ].

		self if: selectorParts in: ast includes: #classVariableNames do: [:argument |
			classVariableNames := argument value. ].
		
		self if: selectorParts in: ast includes: #category do: [:argument |
			categoryName := argument value. ].

		self if: selectorParts in: ast includes: #package do: [:argument |
			packageName := argument value. ].
		
		self if: selectorParts in: ast includes: #poolDictionaries do: [:argument |
			poolDictionariesNames := argument value. ].

		self if: selectorParts in: ast includes: #layout do: [:argument |
			layoutDefinition := argument formattedCode. ].

		self if: selectorParts in: ast includes: #slots do: [:argument |
			slotsDefinition := argument "use AST directly". ].

		self if: selectorParts in: ast includes: #uses do: [:argument |
			traitsDefinition := argument formattedCode. ].
		
		hasNilSuperclass ifTrue: [superclassName := nil].
		
		(#(#instanceVariableNames: uses: uses:instanceVariableNames:) includes: ast selector) ifTrue: [ 			subclassName := ast receiver formattedCode asSymbol.
			superclassName := nil ].
		
		(#(CompiledBlock CompiledCode CompiledMethod) includes: subclassName) ifTrue: [ 
			layoutClass := RG2CompiledMethodLayoutDefinition ].
		
		^ self createBehavior: subclassName
			superclassName: superclassName 
			instanceVariableNames: instanceVariableNames 
			classVariableNames: classVariableNames 
			categoryName: categoryName 
			packageName: packageName 
			poolDictionariesNames: poolDictionariesNames 
			layoutClass: layoutClass 
			layoutDefinition: layoutDefinition 
			slotsDefinition: slotsDefinition 
			traits: traitsDefinition 
			isTrait: isTrait
			hasNilSuperclass: hasNilSuperclass.
	].

	doIts add: aChunk. 
]

{ #category : #visitor }
Ring2ChunkImporter >> visitMethodChunk: aChunk [

	| theClass theMethod theProtocol |

	theClass := self classNamed: aChunk behaviorName. 
	aChunk isMeta ifTrue: [ 
		theClass := theClass theMetaClass makeResolved].
	
	theMethod := theClass ensureLocalMethodNamed: aChunk methodSelector asSymbol.
	theProtocol := theClass ensureProtocolNamed: aChunk category asSymbol.
	theMethod protocol: theProtocol.
	theMethod sourceCode: aChunk contents.
	theMethod author: (theMethod authorForStamp: aChunk stamp).
	theMethod time: (theMethod timeForStamp: aChunk stamp).

]

{ #category : #visitor }
Ring2ChunkImporter >> visitStyleChunk: aChunk [

	"Do nothing with styles"
]
