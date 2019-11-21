Class {
	#name : #RGTraitV2Strategy,
	#superclass : #RGTraitV2DescriptionStrategy,
	#instVars : [
		'comment',
		'classVariables',
		'package',
		'sharedPools',
		'classTrait'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'private - backend access' }
RGTraitV2Strategy >> acceptVisitor: aVisitor [ 

	^ aVisitor visitClass: self owner
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> addClassVariable: anRGInstanceVariableDefinition [ 

	self owner announceDefinitionChangeDuring: [ 
		self privAddClassVariable: anRGInstanceVariableDefinition ]
	
	
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> addSharedPool: anRGPoolVariable [

	self owner announceDefinitionChangeDuring: [ 
		self privAddSharedPool: anRGPoolVariable ].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> allClassVarNames [

	^self allClassVariables collect:[ :cvar| cvar name ]
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> allClassVariables [
	"Answer a collection of the receiver's classVariables, including those defined its superclasses"

	^ ((self owner superclass == nil) or: [ self owner superclass == self owner]) 
		ifTrue: [ self classVariables ]
		ifFalse: [ self owner superclass allClassVariables, classVariables ]
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> bindingOf: varName [

	| aSymbol | 
	aSymbol := varName asSymbol.

	^ (self innerBindingOf: aSymbol) ifNil: [
		 self environment bindingOf: aSymbol ]
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> category [

	^ self owner tags 
		ifEmpty: [ self owner package categoryName ]
		ifNotEmpty: [ 
			(self owner package hasResolvedName) 
				ifTrue: [ self owner package categoryName, '-', self owner tags first  ]
				ifFalse: [ self owner tags first  ]	] 
	
	"todo"
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> category: aString [

	| aTag |
		
	self owner cleanTagsWithoutAnnouncemnt.
	aTag := self owner package 
		ifNotNil: [ 
			self owner package name = aString
				ifTrue: [ ^ self owner ] "category contains only the package name"
				ifFalse: [ aString withoutPrefix: self package name, '-'  ]]
		ifNil: [ aString ].
	self owner tagWith: aTag asSymbol.

"	self backend forBehavior setCategoryFor: self to: aaString.
	
	self announcer behaviorDefinitionModified: self.
	self announcer behaviorRecategorized: self."
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> classSide [

	^ self owner metaclass
]

{ #category : #'as yet unclassified' }
RGTraitV2Strategy >> classTrait [

	^ self backend forBehavior classTraitFor: self owner
]

{ #category : #'as yet unclassified' }
RGTraitV2Strategy >> classTrait: anRGMetatraitDefinition [

	self backend forBehavior setClassTraitFor: self owner to: anRGMetatraitDefinition
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> classVarNames [

	^ self classVariables collect: #name
]

{ #category : #'class variables' }
RGTraitV2Strategy >> classVariableDefinitionString [
	"Answer a string that evaluates to the definition of the class Variables"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self owner classVariables do: [:global |
				str nextPutAll: global definitionString.
				special := global isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> classVariables [

	^ Array streamContents: [ :str | self classVariablesDo: [ :each | str nextPut: each ] ]
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> classVariables: aCollectionOfSymbols [

	self cleanClassVariables.
	aCollectionOfSymbols do: [ :classVarName |
		self addClassVariable: (RGClassVariable named: classVarName asSymbol parent: self).].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> classVariablesDo: aBlock [

	self backend forBehavior classVariablesFor: self owner do: aBlock
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> cleanClassVariables [

	self owner announceDefinitionChangeDuring: [ 
		self backend forBehavior cleanClassVariablesFor: self owner ].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> cleanSharedPools [

	self backend forBehavior cleanSharedPoolsFor: self owner.
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> comment [

	^ self backend forBehavior commentFor: self owner
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> comment: anRGComment [

	self backend forBehavior setCommentFor: self owner to: anRGComment.
	
	self owner announcer behaviorCommentModified: self.
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> copyForBehaviorDefinitionPostCopy [

	| newVariables newSharedPools |
	
	self owner behaviorStrategy: self.
	
	super copyForBehaviorDefinitionPostCopy.
	
	newVariables := self classVariables collect: [ :each | each copyWithParent: self owner].
	newSharedPools := self sharedPools collect: [ :each | each shallowCopy. ].

	self pvtCleanClassVariables.
	self pvtCleanSharedPools.
	
	newVariables do: [ :each | self privAddClassVariable: each ].
	newSharedPools do: [ :each | self privAddSharedPool: each ].



]

{ #category : #'default model values' }
RGTraitV2Strategy >> defaultClassVariables [

	^ OrderedCollection new

	
]

{ #category : #'default model values' }
RGTraitV2Strategy >> defaultComment [

	^ self owner defaultCommentStubIn: self owner
]

{ #category : #testing }
RGTraitV2Strategy >> defaultMetaClass [

	^ self owner environment backend createNewUnresolvedMetaclassFor: self owner
]

{ #category : #'default model values' }
RGTraitV2Strategy >> defaultPackage [

	^ self owner defaultPackageStubIn: self environment

	
]

{ #category : #'default model values' }
RGTraitV2Strategy >> defaultSharedPools [

	^ OrderedCollection new

	
]

{ #category : #testing }
RGTraitV2Strategy >> definition [
	"Answer a String that defines the receiver"

	^String streamContents: [:stream |
		stream nextPutAll: 'Trait'.
		stream nextPutAll: ' named: ';
				store: self owner name.
		stream cr; tab; nextPutAll: 'uses: ';
				nextPutAll: self owner traitCompositionString.
		(self owner slots size > 0) ifTrue: [ 
			stream
				crtab;
				nextPutAll: 'slots: ';
				nextPutAll: self owner slotDefinitionString].
		stream cr; tab; nextPutAll: 'package: ';
				store: self category asString].
]

{ #category : #testing }
RGTraitV2Strategy >> definitionWithSlots [

	| aStream poolString|

	poolString := self owner sharedPoolsString.

	aStream := (String new: 800) writeStream.
	self owner superclass 
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: self owner superclass name].
	aStream nextPutAll: ' subclass: ';
			store: self owner name.
	(self owner hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: self owner traitCompositionString].		
			
	(self layout layoutName = #FixedLayout) ifFalse: [
		aStream cr; tab; nextPutAll: 'layout: ';
			nextPutAll: self layout layoutName].
	aStream cr; tab; nextPutAll: 'slots: ';
			nextPutAll: self owner slotDefinitionString.
	aStream cr; tab; nextPutAll: 'classVariables: ';
			nextPutAll: self owner classVariableDefinitionString.
	poolString = '' ifFalse: [
		aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: poolString].
	aStream cr; tab; nextPutAll: 'package: ';
			store: self category asString.

	self owner superclass ifNil: [ 
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: self owner name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents
]

{ #category : #initialization }
RGTraitV2Strategy >> initialize [

	super initialize.
	
	comment := self unresolvedValue: self defaultComment.	
	classVariables := self unresolvedValue: self defaultClassVariables.	
	package := self unresolvedValue: self defaultPackage.	
	sharedPools := self unresolvedValue: self defaultSharedPools.	
			
	
]

{ #category : #initialization }
RGTraitV2Strategy >> initializeUnresolved [

	super initializeUnresolved.
	
	comment := self unresolvedValue: self defaultComment.	
	classVariables := self unresolvedValue: self defaultClassVariables.	
	package := self unresolvedValue: self defaultPackage.	
	sharedPools := self unresolvedValue: self defaultSharedPools.	
	
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> innerBindingOf: aSymbol [

	self classVariables detect: [ :each | each name = aSymbol  ] ifFound: [ :found | ^ found ].

	self sharedPoolsDo: [:pool | 
		((self environment ask behaviorNamed: pool name) bindingOf: aSymbol) ifNotNil: [:binding | ^binding]].

	((self owner superclass == self owner) or: [ self owner superclass isNil ]) ifFalse: [ ^ self owner superclass innerBindingOf: aSymbol].	
		
	^ nil
]

{ #category : #testing }
RGTraitV2Strategy >> isClass [

	^ true
]

{ #category : #testing }
RGTraitV2Strategy >> isClassStrategy [

	^ false
]

{ #category : #'default model values' }
RGTraitV2Strategy >> makeResolved [

	
	"try to set the correct name before resolving of it"
	((self owner hasResolvedName not) and: [ self owner metaclass isRingResolved and: [ self metaclass hasResolvedName ] ]) ifTrue: [ 
		self owner pvtName: (self owner metaclass name withoutSuffix: ' class') asSymbol.
		 ].
	
	super makeResolved.
	
	classTrait := self classTrait markAsRingResolved.
	comment := self comment markAsRingResolved.
	classVariables := self classVariables markAsRingResolved.
	package := self package markAsRingResolved.
	sharedPools := self sharedPools markAsRingResolved.

]

{ #category : #'accessing - backend' }
RGTraitV2Strategy >> package [

	^ self backend forBehavior packageFor: self owner
]

{ #category : #'accessing - backend' }
RGTraitV2Strategy >> package: anRGPackage [

	| oldPackage |
	oldPackage := self package.

	self owner announceDefinitionChangeDuring: [ 
		self owner backend forBehavior setPackageFor: self owner to: anRGPackage.
		self owner environment addPackage: anRGPackage.
		(oldPackage hasResolved: #definedBehaviors)
			ifTrue: [ oldPackage removeDefinedBehavior: self owner ].
		anRGPackage addDefinedBehavior: self owner. ].
	
	self owner announce: (ClassRepackaged 
		classRepackaged: self owner
		oldPackage: oldPackage
		newPackage: anRGPackage)
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> privAddClassVariable: anRGInstanceVariableDefinition [ 

	self backend forBehavior addClassVariable: anRGInstanceVariableDefinition to: self owner	
	
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> privAddSharedPool: anRGPoolVariable [
	
	self backend forBehavior addSharedPool: anRGPoolVariable to: self owner
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtAddClassVariable: anRGInstanceVariableDefinition [

	self owner environment verifyOwnership: anRGInstanceVariableDefinition.
	
	classVariables isRingResolved ifFalse: [ 
		self pvtCleanClassVariables  ].
	
	classVariables add: anRGInstanceVariableDefinition.	
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtAddSharedPool: anRGPoolVariable [

	self owner environment verifyOwnership: anRGPoolVariable.
	
	sharedPools isRingResolved ifFalse: [ 
		self pvtCleanSharedPools  ].
	
	sharedPools add: anRGPoolVariable.	
]

{ #category : #testing }
RGTraitV2Strategy >> pvtClassTrait [

	^ classTrait value
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtClassTrait: anRGMetatraitDefinition [

	self owner environment verifyOwnership: anRGMetatraitDefinition.

	^ classTrait := anRGMetatraitDefinition
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtClassVariablesDo: aBlock [

	classVariables value do: aBlock
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtCleanClassVariables [

	classVariables := self defaultClassVariables.
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtCleanSharedPools [

	sharedPools := self defaultSharedPools.
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtComment [

	^ comment value
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtComment: anRGComment [

	self owner environment verifyOwnership: anRGComment.

	^ comment := anRGComment
]

{ #category : #testing }
RGTraitV2Strategy >> pvtPackage [

	^ package value
]

{ #category : #testing }
RGTraitV2Strategy >> pvtPackage: anRGPackage [

	self owner environment verifyOwnership: anRGPackage.

	^ package := anRGPackage
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtRemoveClassVariable: anRGInstanceVariableDefinition [

	self owner environment verifyOwnership: anRGInstanceVariableDefinition.
	
	classVariables remove: anRGInstanceVariableDefinition.
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> pvtRemoveSharedPool: anRGPoolVariableDefinition [

	self owner environment verifyOwnership: anRGPoolVariableDefinition.
	
	sharedPools remove: anRGPoolVariableDefinition.
]

{ #category : #initialization }
RGTraitV2Strategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#comment -> comment. 
		#classVariables -> classVariables. 
		#package -> package. 
		#sharedPools -> sharedPools. 
	}
	
	

]

{ #category : #enumerating }
RGTraitV2Strategy >> pvtSharedPoolsDo: aBlock [

	sharedPools value do: aBlock
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> removeClassVariable: anRGInstanceVariableDefinition [

	self owner announceDefinitionChangeDuring: [ 
		self backend forBehavior removeClassVariable: anRGInstanceVariableDefinition from: self owner ].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> removeSharedPool: anRGPoolVariable [

	self owner announceDefinitionChangeDuring: [ 
		self backend forBehavior removeSharedPool: anRGPoolVariable from: self owner].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> sharedPoolNames [

	| allSharedPools |
	allSharedPools := OrderedCollection new.
	self sharedPoolsDo: [ :each | allSharedPools add: each name].
	^ allSharedPools asArray
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> sharedPools [

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

{ #category : #'private - backend access' }
RGTraitV2Strategy >> sharedPools: aCollectionOfSymbols [

	self cleanSharedPools.
	aCollectionOfSymbols do: [ :poolName |
		self addSharedPool: (RGPoolVariable named: poolName asSymbol parent: self).].
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> sharedPoolsDo: aBlock [

	self owner backend forBehavior sharedPoolsFor: self owner do: aBlock
]

{ #category : #'private - backend access' }
RGTraitV2Strategy >> sibling [

	^ self owner metaclass.
]

{ #category : #testing }
RGTraitV2Strategy >> storeName [

	^ 'RGClass'
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGTraitV2Strategy >> theMetaClass [

	^ self owner metaclass
]
