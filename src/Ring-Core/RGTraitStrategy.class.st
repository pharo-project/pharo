Class {
	#name : #RGTraitStrategy,
	#superclass : #RGTraitDescriptionStrategy,
	#instVars : [
		'classTrait',
		'comment',
		'package'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'private - backend access' }
RGTraitStrategy >> acceptVisitor: aVisitor [ 

	^ aVisitor visitTrait: self owner
]

{ #category : #'private - backend access' }
RGTraitStrategy >> baseTrait [

	^ self owner
]

{ #category : #'private - backend access' }
RGTraitStrategy >> category [

	^ self owner tags 
		ifEmpty: [ self owner package categoryName ]
		ifNotEmpty: [ 
			(self package hasResolvedName) 
				ifTrue: [ self owner package categoryName, '-', self owner tags first  ]
				ifFalse: [ self owner tags first  ]	] 
	
	"todo"
]

{ #category : #'private - backend access' }
RGTraitStrategy >> category: aString [

	| aTag |
		
	self owner cleanTags.
	aTag := self package 
		ifNotNil: [ 
			self package name = aString
				ifTrue: [ ^ self ] "category contains only the package name"
				ifFalse: [ aString withoutPrefix: self package name, '-'  ]]
		ifNil: [ aString ].
	self owner tagWith: aTag asSymbol.

"	self backend forBehavior setCategoryFor: self to: aaString.
	
	self announcer behaviorDefinitionModified: self.
	self announcer behaviorRecategorized: self."
]

{ #category : #testing }
RGTraitStrategy >> classSide [

	^ self classTrait
]

{ #category : #'private - backend access' }
RGTraitStrategy >> classTrait [

	^ self backend forBehavior classTraitFor: self owner
]

{ #category : #'private - backend access' }
RGTraitStrategy >> classTrait: anRGMetatraitDefinition [

	self backend forBehavior setClassTraitFor: self owner to: anRGMetatraitDefinition
]

{ #category : #testing }
RGTraitStrategy >> comment [

	^ self backend forBehavior traitCommentFor: self owner
]

{ #category : #testing }
RGTraitStrategy >> comment: anRGComment [

	self backend forBehavior setTraitCommentFor: self owner to: anRGComment.
	
	self owner announcer behaviorCommentModified: self owner.
]

{ #category : #testing }
RGTraitStrategy >> defaultCategory [

	^ nil
]

{ #category : #testing }
RGTraitStrategy >> defaultComment [

	^ self owner defaultCommentStubIn: self owner
]

{ #category : #testing }
RGTraitStrategy >> defaultPackage [

	^ self owner defaultPackageStubIn: self environment

	
]

{ #category : #testing }
RGTraitStrategy >> definition [
	"Answer a String that defines the receiver"

	^String streamContents: [:stream |
		stream nextPutAll: 'Trait'.
		stream nextPutAll: ' named: ';
				store: self owner name.
		stream cr; tab; nextPutAll: 'uses: ';
				nextPutAll: self owner traitCompositionString.
		stream cr; tab; nextPutAll: 'package: ';
				store: self owner category asString].
]

{ #category : #testing }
RGTraitStrategy >> hasComment [

	^ self comment isEmptyOrNil not
]

{ #category : #testing }
RGTraitStrategy >> initialize [

	super initialize.
	
	"classTrait is set in the superclass"
	comment := self unresolvedValue: self defaultComment.
	package := self unresolvedValue: self defaultPackage.

]

{ #category : #testing }
RGTraitStrategy >> initializeUnresolved [

	super initializeUnresolved.
	
	classTrait := RGUnresolvedValue recursive.
	comment := self unresolvedValue: self defaultComment.
	package := self unresolvedValue: self defaultPackage.

]

{ #category : #testing }
RGTraitStrategy >> instanceSide [
	^ self baseTrait
]

{ #category : #testing }
RGTraitStrategy >> isTraitStrategy [

	^ true
]

{ #category : #accessing }
RGTraitStrategy >> makeResolved [

	
	"try to set the correct name before resolving of it"
	((self owner hasResolvedName not) and: [ self classTrait isRingResolved and: [ self classTrait hasResolvedName ] ]) ifTrue: [ 
		self owner pvtName: (self classTrait name withoutSuffix: ' classTrait') asSymbol.
		 ].
	
	super makeResolved.
	classTrait := self classTrait markAsRingResolved.
	comment := self comment markAsRingResolved.
	package := self package markAsRingResolved.
	
]

{ #category : #accessing }
RGTraitStrategy >> package [

	^ self backend forBehavior traitPackageFor: self owner
]

{ #category : #accessing }
RGTraitStrategy >> package: anRGPackage [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setTraitPackageFor: self to: anRGPackage.
		self environment addPackage: anRGPackage.
		anRGPackage addDefinedBehavior: self owner. ].
]

{ #category : #accessing }
RGTraitStrategy >> pvtClassTrait [

	^ classTrait value
]

{ #category : #accessing }
RGTraitStrategy >> pvtClassTrait: anRGMetatraitDefinition [

	self owner environment verifyOwnership: anRGMetatraitDefinition.

	^ classTrait := anRGMetatraitDefinition
]

{ #category : #accessing }
RGTraitStrategy >> pvtComment [

	^ comment value
]

{ #category : #accessing }
RGTraitStrategy >> pvtComment: anRGComment [

	self owner environment verifyOwnership: anRGComment.

	^ comment := anRGComment
]

{ #category : #accessing }
RGTraitStrategy >> pvtPackage [

	^ package value
]

{ #category : #accessing }
RGTraitStrategy >> pvtPackage: anRGPackage [

	self owner environment verifyOwnership: anRGPackage.

	^ package := anRGPackage
]

{ #category : #accessing }
RGTraitStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#classTrait -> classTrait. 
		#comment -> comment. 
		#package -> package. 
   	}
	
	
]

{ #category : #accessing }
RGTraitStrategy >> referencedBehaviors [

	^ super referencedBehaviors, {self classTrait}.
]

{ #category : #accessing }
RGTraitStrategy >> referencedPackages [

	^ Array with: package
]

{ #category : #accessing }
RGTraitStrategy >> sibling [

	^ self classTrait
]

{ #category : #testing }
RGTraitStrategy >> storeName [

	^ 'RGTrait'
]

{ #category : #accessing }
RGTraitStrategy >> theMetaClass [
	^ self classTrait
]

{ #category : #accessing }
RGTraitStrategy >> traitTransformationString [

	^ self owner name
]
