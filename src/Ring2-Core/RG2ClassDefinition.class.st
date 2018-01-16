"
RG2ClassDefinition is the concrete representation of a class (no trait)
"
Class {
	#name : #RG2ClassDefinition,
	#superclass : #RG2ClassDescriptionDefinition,
	#instVars : [
		'comment',
		'classVariables',
		'package',
		'sharedPools'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2ClassDefinition >> addClassVariable: aRG2InstanceVariableDefinition [ 

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior addClassVariable: aRG2InstanceVariableDefinition to: self ]
	
	
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> addSharedPool: aRG2PoolVariableDefinition [ 

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior addSharedPool: aRG2PoolVariableDefinition to: self ].
]

{ #category : #'managing container' }
RG2ClassDefinition >> addoptToParentStub [

	super addoptToParentStub.
	
	

"	aPackage := self environment ensureUnpackagedPackage.
	self pvtPackage: aPackage."

]

{ #category : #'queries - variables' }
RG2ClassDefinition >> allClassVarNames [

	^self allClassVariables collect:[ :cvar| cvar name ]
]

{ #category : #'queries - variables' }
RG2ClassDefinition >> allClassVariables [
	"Answer a collection of the receiver's classVariables, including those defined its superclasses"

	^ ((self superclass == nil) or: [ self superclass == self ]) 
		ifTrue: [ self classVariables ]
		ifFalse: [ self superclass allClassVariables, classVariables ]
]

{ #category : #binding }
RG2ClassDefinition >> bindingOf: varName [

	| aSymbol | 
	aSymbol := varName asSymbol.

	^ (self innerBindingOf: aSymbol) ifNil: [
		 self environment bindingOf: aSymbol ]
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> canMergeWith: aRG2Definition visited: visitedDefinitions [

	(super canMergeWith: aRG2Definition visited: visitedDefinitions) ifFalse: [ ^ false ].
	
	"TODO: other elements? collections?"

	((self hasResolved: #comment) and: [aRG2Definition hasResolved: #comment])
		ifTrue: [	(self comment canMergeWith: aRG2Definition comment visited: visitedDefinitions ) ifFalse: [^ false]].

	((self hasResolved: #category) and: [aRG2Definition hasResolved: #category])
		ifTrue: [	(self category ~= aRG2Definition category) ifTrue: [^ false]].

	^ true.
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> category [

	^ self tags 
		ifEmpty: [ self package categoryName ]
		ifNotEmpty: [ 
			(self package hasResolved: #name) 
				ifTrue: [ self package categoryName, '-', self tags first  ]
				ifFalse: [ self tags first  ]	] 
	
	"todo"
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> category: aString [

	| aTag |
		
	self cleanTagsWithoutAnnouncemnt.
	aTag := self package 
		ifNotNil: [ 
			self package name = aString
				ifTrue: [ ^ self ] "category contains only the package name"
				ifFalse: [ aString withoutPrefix: self package name, '-'  ]]
		ifNil: [ aString ].
	self tagWith: aTag asSymbol.

"	self backend forBehavior setCategoryFor: self to: aaString.
	
	self announcer behaviorDefinitionModified: self.
	self announcer behaviorRecategorized: self."
]

{ #category : #children }
RG2ClassDefinition >> childDefinitionsDo: aBlock traversed: aSet [

	self pvtComment withChildrenDo: aBlock traversed: aSet.
	self pvtClassVariablesDo: [ :each | each withChildrenDo: aBlock traversed: aSet ].
	self pvtPackage withChildrenDo: aBlock traversed: aSet.
	self pvtSharedPoolsDo: [ :each | each withChildrenDo: aBlock traversed: aSet ].
	
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> classSide [

	^ self metaclass
]

{ #category : #'queries - variables' }
RG2ClassDefinition >> classVarNames [

	^ self classVariables collect: #name
]

{ #category : #'class variables' }
RG2ClassDefinition >> classVariableDefinitionString [
	"Answer a string that evaluates to the definition of the class Variables"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self classVariables do: [:global |
				str nextPutAll: global definitionString.
				special := global isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]
]

{ #category : #'queries - variables' }
RG2ClassDefinition >> classVariables [

	| allClassVariables |
	allClassVariables := OrderedCollection new.
	self classVariablesDo: [ :each | allClassVariables add: each].
	^ allClassVariables asArray
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> classVariables: aCollectionOfSymbols [

	self cleanClassVariables.
	aCollectionOfSymbols do: [ :classVarName |
		self addClassVariable: (RG2ClassVariableDefinition named: classVarName asSymbol parent: self).].
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> classVariablesDo: aBlock [

	self backend forBehavior classVariablesFor: self do: aBlock
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> cleanClassVariables [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanClassVariablesFor: self ].
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> cleanSharedPools [

	self backend forBehavior cleanSharedPoolsFor: self.
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> comment [

	^ self backend forBehavior commentFor: self
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> comment: aRG2CommentDefinition [

	self backend forBehavior setCommentFor: self to: aRG2CommentDefinition.
	
	self announcer behaviorCommentModified: self.
]

{ #category : #copying }
RG2ClassDefinition >> copyForBehaviorDefinitionPostCopy [

	| newVariables newSharedPools |
	super copyForBehaviorDefinitionPostCopy.
	
	newVariables := self classVariables collect: [ :each | each copyWithParent: self ].
	newSharedPools := self sharedPools collect: [ :each | each shallowCopy. ].

	self pvtCleanClassVariables.
	self pvtCleanSharedPools.
	
	newVariables do: [ :each | self addClassVariable: each ].
	newSharedPools do: [ :each | self addSharedPool: each ].



]

{ #category : #'default model values' }
RG2ClassDefinition >> defaultCategory [

	^ nil
]

{ #category : #'default model values' }
RG2ClassDefinition >> defaultClassVariables [

	^ OrderedCollection new

	
]

{ #category : #'default model values' }
RG2ClassDefinition >> defaultComment [

	^ self defaultCommentStubIn: self
]

{ #category : #'default model values' }
RG2ClassDefinition >> defaultPackage [

	^ self defaultPackageStubIn: self environment

	
]

{ #category : #'default model values' }
RG2ClassDefinition >> defaultSharedPools [

	^ OrderedCollection new

	
]

{ #category : #accessing }
RG2ClassDefinition >> definition [

	| aStream poolString |
	
	self usesSpecialSlot 
		ifTrue: [ ^ self definitionWithSlots ].
		
	poolString := self sharedPoolsString.
	aStream := (String new: 800) writeStream.
	superclass
		ifNil: [ aStream nextPutAll: 'ProtoObject' ]
		ifNotNil: [ aStream nextPutAll: superclass name ].
	aStream
		nextPutAll: self kindOfSubclass;
		store: self name.
	self hasTraitComposition
		ifTrue:
			[ aStream
				cr;
				tab;
				nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString ].
	aStream
		cr;
		tab;
		nextPutAll: 'instanceVariableNames: ';
		store: self instanceVariablesString.
	aStream
		cr;
		tab;
		nextPutAll: 'classVariableNames: ';
		store: self classVariablesString.
	poolString = ''
		ifFalse:
			[ aStream
				cr;
				tab;
				nextPutAll: 'poolDictionaries: ';
				store: poolString ].
	aStream
		cr;
		tab;
		nextPutAll: 'package: ';
		store: self category asString.
	superclass
		ifNil:
			[ aStream
				nextPutAll: '.';
				cr.
			aStream nextPutAll: self name.
			aStream
				space;
				nextPutAll: 'superclass: nil' ].
	^ aStream contents
]

{ #category : #accessing }
RG2ClassDefinition >> definitionWithSlots [

	| aStream poolString|

	poolString := self sharedPoolsString.

	aStream := (String new: 800) writeStream.
	self superclass 
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: self superclass name].
	aStream nextPutAll: ' subclass: ';
			store: self name.
	(self hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString].		
			
	(self layout isKindOf: FixedLayout) ifFalse: [
		aStream cr; tab; nextPutAll: 'layout: ';
			nextPutAll: self layout class name].
	aStream cr; tab; nextPutAll: 'slots: ';
			nextPutAll: self slotDefinitionString.
	aStream cr; tab; nextPutAll: 'classVariables: ';
			nextPutAll: self classVariableDefinitionString.
	poolString = '' ifFalse: [
		aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: poolString].
	aStream cr; tab; nextPutAll: 'category: ';
			store: self category asString.

	self superclass ifNil: [ 
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: self name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents
]

{ #category : #testing }
RG2ClassDefinition >> hasComment [

	^ self comment isEmptyOrNil not
]

{ #category : #initialization }
RG2ClassDefinition >> initialize [

	super initialize.
	
	comment := self unresolvedValue: self defaultComment.	
	classVariables := self unresolvedValue: self defaultClassVariables.	
	package := self unresolvedValue: self defaultPackage.	
	sharedPools := self unresolvedValue: self defaultSharedPools.	
			
	
]

{ #category : #initialization }
RG2ClassDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	comment := self unresolvedValue: self defaultComment.	
	classVariables := self unresolvedValue: self defaultClassVariables.	
	package := self unresolvedValue: self defaultPackage.	
	sharedPools := self unresolvedValue: self defaultSharedPools.	
	
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> innerBindingOf2: aSymbol [

	((self classVariables collect: #name) includes: aSymbol)
		ifTrue: [ | binding |
			binding := self classVariablesBindings bindingOf: aSymbol.
			^ binding ifNil: [ 
				self classVariablesBindings at: aSymbol put: (self classVariables detect: [:each | each name = aSymbol]). 
				^  self classVariablesBindings bindingOf: aSymbol.]].
	
	self sharedPoolsDo: [:pool | 
		((self environment ask behaviorNamed: pool name) bindingOf: aSymbol) ifNotNil: [:binding | ^binding]].

	((self superclass == self) or: [ self superclass isNil ]) ifFalse: [ ^ self superclass innerBindingOf: aSymbol].	
		
	^ nil
]

{ #category : #compiling }
RG2ClassDefinition >> innerBindingOf: aSymbol [

	self classVariables detect: [ :each | each name = aSymbol  ] ifFound: [ :found | ^ found ].

	self sharedPoolsDo: [:pool | 
		((self environment ask behaviorNamed: pool name) bindingOf: aSymbol) ifNotNil: [:binding | ^binding]].

	((self superclass == self) or: [ self superclass isNil ]) ifFalse: [ ^ self superclass innerBindingOf: aSymbol].	
		
	^ nil
]

{ #category : #'testing types' }
RG2ClassDefinition >> isClass [
	
	^ true
]

{ #category : #'queries - tags' }
RG2ClassDefinition >> isTaggedWith: aSymbol [

	^self tags includes: aSymbol 
]

{ #category : #resolving }
RG2ClassDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	self resolveName.
	
	super makeMinimallyResolved.
	
	comment := self comment.
	classVariables := self classVariables collect: #makeMinimallyResolved.
	package := self package makeMinimallyResolved.
	sharedPools := self sharedPools collect: #makeMinimallyResolved.

]

{ #category : #resolving }
RG2ClassDefinition >> makeResolved [

	self resolveName.
	
	super makeResolved.
	
	self resolveComment.
	self resolveClassVariables.
	self resolvePackage.
	self resolveSharedPools.

]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> package [

	^ self backend forBehavior packageFor: self
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> package: aRG2Package [

	| oldPackage |
	oldPackage := self package.

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setPackageFor: self to: aRG2Package.
		self environment addPackage: aRG2Package.
		oldPackage removeDefinedBehavior: self.
		aRG2Package addDefinedBehavior: self. ].
	
	self announce: (ClassRepackaged 
		classRepackaged: self
		oldPackage: oldPackage
		newPackage: aRG2Package)
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtAddClassVariable: aRG2InstanceVariableDefinition [

	self environment verifyOwnership: aRG2InstanceVariableDefinition.
	
	classVariables isRing2Resolved ifFalse: [ 
		self pvtCleanClassVariables  ].
	
	classVariables add: aRG2InstanceVariableDefinition.	
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtAddSharedPool: aRG2PoolVariableDefinition [

	self environment verifyOwnership: aRG2PoolVariableDefinition.
	
	sharedPools isRing2Resolved ifFalse: [ 
		self pvtCleanSharedPools  ].
	
	sharedPools add: aRG2PoolVariableDefinition.	
]

{ #category : #private }
RG2ClassDefinition >> pvtAsTrait [

	| newTrait |
	
	(self resolvedProperties includesAnyOf: #(superclass traitComposition classVariables sharedPools))
		ifTrue: [self error: 'This class cannot be converted to trait'].
		
	"TODO: check empty layout"
	newTrait := RG2TraitDefinition unresolvedWithParent: self parent.
	newTrait copyPropertiesFrom: self.
	newTrait pvtName: name.
	newTrait pvtLocalMethods: localMethods.
	newTrait pvtComment: comment.
	newTrait pvtPackage: package.
	newTrait pvtTags: tags.
	
	^ newTrait
	
	
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtClassVariablesDo: aBlock [

	classVariables value do: aBlock
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtCleanClassVariables [

	classVariables := self defaultClassVariables.
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtCleanSharedPools [

	sharedPools := self defaultSharedPools.
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtComment [

	^ comment value
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtComment: aRG2CommentDefinition [

	self environment verifyOwnership: aRG2CommentDefinition.

	^ comment := aRG2CommentDefinition
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtPackage [

	^ package value
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtPackage: aRG2PackageDefinition [

	self environment verifyOwnership: aRG2PackageDefinition.

	^ package := aRG2PackageDefinition
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtRemoveClassVariable: aRG2InstanceVariableDefinition [

	self environment verifyOwnership: aRG2InstanceVariableDefinition.
	
	classVariables remove: aRG2InstanceVariableDefinition.
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtRemoveSharedPool: aRG2PoolVariableDefinition [

	self environment verifyOwnership: aRG2PoolVariableDefinition.
	
	sharedPools remove: aRG2PoolVariableDefinition.
]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#metaclass -> metaclass. 
		#localMethods -> localMethods. 
		#comment -> comment. 
		#classVariables -> classVariables. 
		#package -> package. 
		#sharedPools -> sharedPools. 
	}
	
	

]

{ #category : #'private - backend access' }
RG2ClassDefinition >> pvtSharedPoolsDo: aBlock [

	sharedPools value do: aBlock
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> referencedPackages [

	^ Array with: self package.
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> removeClassVariable: aRG2InstanceVariableDefinition [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior removeClassVariable: aRG2InstanceVariableDefinition from: self ].
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> removeSharedPool: aRG2PoolVariableDefinition [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior removeSharedPool: aRG2PoolVariableDefinition from: self ].
]

{ #category : #resolving }
RG2ClassDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.

	self resolveComment. 
	self comment resolveBehaviorInformation.

	self resolveClassVariables.
	self classVariablesDo: #resolveBehaviorInformation.

	self resolvePackage.
	self package resolveBehaviorInformation.
	
	self resolveSharedPoolNames.
]

{ #category : #resolving }
RG2ClassDefinition >> resolveClassVariables [

	classVariables := self classVariables.
]

{ #category : #resolving }
RG2ClassDefinition >> resolveComment [

	comment := self comment.
]

{ #category : #resolving }
RG2ClassDefinition >> resolvePackage [

	package := self package.
]

{ #category : #resolving }
RG2ClassDefinition >> resolveSharedPoolNames [

	self resolveSharedPools.
	self sharedPools do: [:each | each resolveName ].
]

{ #category : #resolving }
RG2ClassDefinition >> resolveSharedPools [

	sharedPools := self sharedPools.
]

{ #category : #resolving }
RG2ClassDefinition >> resolveSibling [

	self resolveMetaclass.
	self metaclass resolveBehaviorInformation.
]

{ #category : #resolving }
RG2ClassDefinition >> setNameFromSiblingIfNeeded [

	(self hasUnresolved: #name)
		ifTrue: [ name := (self metaclass name withoutSuffix: ' class') asSymbol ]

]

{ #category : #'queries - shared pools' }
RG2ClassDefinition >> sharedPoolNames [

	| allSharedPools |
	allSharedPools := OrderedCollection new.
	self sharedPoolsDo: [ :each | allSharedPools add: each name].
	^ allSharedPools asArray
]

{ #category : #'queries - shared pools' }
RG2ClassDefinition >> sharedPools [

	| allSharedPools |
	allSharedPools := OrderedCollection new.
	self sharedPoolsDo: [ :each | allSharedPools add: (
		"use classes everywhare you can. TODO: really?"
		each isSymbol 
			ifTrue: [ self environment ask behaviorNamed: each name ]
			ifFalse: [ 
				(each isVariableDefinition and: [each isPoolVariable ])
					ifTrue: [ 
						| beh | 
						beh := self environment ask behaviorNamed: each name.
						beh ifNotNil: [ beh ] ifNil: [ each ] ]
					ifFalse: [ each copy ]])].
	^ allSharedPools 
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> sharedPools: aCollectionOfSymbols [

	self cleanSharedPools.
	aCollectionOfSymbols do: [ :poolName |
		self addSharedPool: (RG2PoolVariableDefinition named: poolName asSymbol parent: self).].
]

{ #category : #'accessing - backend' }
RG2ClassDefinition >> sharedPoolsDo: aBlock [

	self backend forBehavior sharedPoolsFor: self do: aBlock
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> sibling [

	^ self metaclass.
]

{ #category : #'as yet unclassified' }
RG2ClassDefinition >> theMetaClass [
	^ self metaclass
]
