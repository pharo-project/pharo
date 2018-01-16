"
RG2TraitDefinition is the concrete representation of a trait (no class)
"
Class {
	#name : #RG2TraitDefinition,
	#superclass : #RG2TraitDescriptionDefinition,
	#instVars : [
		'classTrait',
		'comment',
		'package'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #accessing }
RG2TraitDefinition >> baseTrait [

	^ self
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> category [

	^ self tags 
		ifEmpty: [ self package categoryName ]
		ifNotEmpty: [ 
			(self package hasResolved: #name) 
				ifTrue: [ self package categoryName, '-', self tags first  ]
				ifFalse: [ self tags first  ]	] 
	
	"todo"
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> category: aString [

	| aTag |
		
	self cleanTags.
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

{ #category : #'accessing - backend' }
RG2TraitDefinition >> childDefinitionsDo: aBlock traversed: aSet [

	self pvtClassTrait withChildrenDo: aBlock traversed: aSet.
	self pvtComment withChildrenDo: aBlock traversed: aSet.
	self pvtPackage withChildrenDo: aBlock traversed: aSet.
	
]

{ #category : #accessing }
RG2TraitDefinition >> classSide [

	^ self classTrait
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> classTrait [

	^ self backend forBehavior classTraitFor: self
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> classTrait: aRG2MetatraitDefinition [

	self backend forBehavior setClassTraitFor: self to: aRG2MetatraitDefinition
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> comment [

	^ self backend forBehavior traitCommentFor: self
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> comment: aRG2CommentDefinition [

	self backend forBehavior setTraitCommentFor: self to: aRG2CommentDefinition.
	
	self announcer behaviorCommentModified: self.
]

{ #category : #'default model values' }
RG2TraitDefinition >> defaultCategory [

	^ nil
]

{ #category : #'default model values' }
RG2TraitDefinition >> defaultComment [

	^ self defaultCommentStubIn: self
]

{ #category : #'default model values' }
RG2TraitDefinition >> defaultPackage [

	^ self defaultPackageStubIn: self environment

	
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> definition [
	"Answer a String that defines the receiver"

	^String streamContents: [:stream |
		stream nextPutAll: 'Trait'.
		stream nextPutAll: ' named: ';
				store: self name.
		stream cr; tab; nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString.
		stream cr; tab; nextPutAll: 'category: ';
				store: self category asString].
]

{ #category : #testing }
RG2TraitDefinition >> hasComment [

	^ self comment isEmptyOrNil not
]

{ #category : #initialization }
RG2TraitDefinition >> initialize [

	super initialize.
	
	"classTrait is set in the superclass"
	comment := self unresolvedValue: self defaultComment.
	package := self unresolvedValue: self defaultPackage.

]

{ #category : #initialization }
RG2TraitDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	classTrait := RG2UnresolvedValue recursive.
	comment := self unresolvedValue: self defaultComment.
	package := self unresolvedValue: self defaultPackage.

]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> instanceSide [
	^ self baseTrait
]

{ #category : #resolving }
RG2TraitDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	self resolveName.
	
	super makeMinimallyResolved.
	classTrait := self classTrait makeMinimallyResolved.
	comment := self comment.
	package := self package makeMinimallyResolved.
	
]

{ #category : #resolving }
RG2TraitDefinition >> makeResolved [

	self resolveName.
	
	super makeResolved.
	
	self resolveClassTrait.
	self resolveComment.
	self resolvePackage.
	
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> package [

	^ self backend forBehavior traitPackageFor: self
]

{ #category : #'accessing - backend' }
RG2TraitDefinition >> package: aRG2Package [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setTraitPackageFor: self to: aRG2Package.
		self environment addPackage: aRG2Package.
		aRG2Package addDefinedBehavior: self. ].
]

{ #category : #'private - backend interface' }
RG2TraitDefinition >> pvtClassTrait [

	^ classTrait value
]

{ #category : #'private - backend interface' }
RG2TraitDefinition >> pvtClassTrait: aRG2MetatraitDefinition [

	self environment verifyOwnership: aRG2MetatraitDefinition.

	^ classTrait := aRG2MetatraitDefinition
]

{ #category : #'private - backend access' }
RG2TraitDefinition >> pvtComment [

	^ comment value
]

{ #category : #'private - backend access' }
RG2TraitDefinition >> pvtComment: aRG2CommentDefinition [

	self environment verifyOwnership: aRG2CommentDefinition.

	^ comment := aRG2CommentDefinition
]

{ #category : #'private - backend access' }
RG2TraitDefinition >> pvtPackage [

	^ package value
]

{ #category : #'private - backend access' }
RG2TraitDefinition >> pvtPackage: aRG2Package [

	self environment verifyOwnership: aRG2Package.

	^ package := aRG2Package
]

{ #category : #'private - backend interface' }
RG2TraitDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#classTrait -> classTrait. 
		#comment -> comment. 
		#package -> package. 
   	}
	
	
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> referencedBehaviors [

	^ super referencedBehaviors, {self classTrait}.
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> referencedPackages [

	^ Array with: package
]

{ #category : #resolving }
RG2TraitDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.

	self resolveComment. 
	self comment resolveBehaviorInformation.

	self resolvePackage.
	self package resolveBehaviorInformation.

]

{ #category : #resolving }
RG2TraitDefinition >> resolveClassTrait [

	classTrait := self classTrait.
]

{ #category : #resolving }
RG2TraitDefinition >> resolveComment [

	comment := self comment.
]

{ #category : #resolving }
RG2TraitDefinition >> resolvePackage [

	package := self package.
]

{ #category : #resolving }
RG2TraitDefinition >> resolveSibling [

	self resolveClassTrait.
	self classTrait resolveBehaviorInformation.
]

{ #category : #resolving }
RG2TraitDefinition >> setNameFromSiblingIfNeeded [

	(self hasUnresolved: #name)
		ifTrue: [ name := (self classTrait name withoutSuffix: ' classTrait') asSymbol ]
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> sibling [

	^ self classTrait
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> theMetaClass [
	^ self classTrait
]

{ #category : #'as yet unclassified' }
RG2TraitDefinition >> traitTransformationString [

	^ self name
]
