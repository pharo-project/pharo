"
RG2MetatraitDefinition is a concrete representation of metaclasses of traits
"
Class {
	#name : #RG2MetaclassTraitDefinition,
	#superclass : #RG2TraitDescriptionDefinition,
	#instVars : [
		'baseTrait'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> baseBehavior [

	^ self baseTrait
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> baseBehavior: aBehavior [

	^ self baseTrait: aBehavior
]

{ #category : #'accessing - backend' }
RG2MetaclassTraitDefinition >> baseTrait [

	^ self backend forBehavior metaclassTraitBaseTraitFor: self
]

{ #category : #'accessing - backend' }
RG2MetaclassTraitDefinition >> baseTrait: aRG2ClassDefinition [

	self backend forBehavior setMetaclassTraitBaseTraitFor: self to: aRG2ClassDefinition
]

{ #category : #accessing }
RG2MetaclassTraitDefinition >> category [

	^ self baseTrait category
]

{ #category : #accessing }
RG2MetaclassTraitDefinition >> comment [

	^ self baseTrait comment
]

{ #category : #'testing types' }
RG2MetaclassTraitDefinition >> definition [
	^String streamContents: [:stream |
		stream
			nextPutAll: self baseTrait name;
			nextPutAll: ' clasTrait';
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString]
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> instanceSide [

	^ self baseTrait
]

{ #category : #testing }
RG2MetaclassTraitDefinition >> isMeta [

	^true
]

{ #category : #'testing types' }
RG2MetaclassTraitDefinition >> isMetaclassTrait [

	^ true
]

{ #category : #resolving }
RG2MetaclassTraitDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	self resolveName.

	super makeMinimallyResolved.
	
	baseTrait := self baseTrait makeMinimallyResolved.
]

{ #category : #resolving }
RG2MetaclassTraitDefinition >> makeResolved [

	self resolveName.

	super makeResolved.
	
	baseTrait := self baseTrait.
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> package [

	^ self baseTrait package
]

{ #category : #'private - backend access' }
RG2MetaclassTraitDefinition >> pvtBaseTrait [

	^ baseTrait value
]

{ #category : #'private - backend access' }
RG2MetaclassTraitDefinition >> pvtBaseTrait: aRG2TraitDefinition [

	self environment verifyOwnership: aRG2TraitDefinition.

	^ baseTrait := aRG2TraitDefinition
]

{ #category : #'private - backend access' }
RG2MetaclassTraitDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#baseTrait -> baseTrait. 
   	}
	
	
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> referencedBehaviors [

	^ super referencedBehaviors, {self baseTrait}
]

{ #category : #resolving }
RG2MetaclassTraitDefinition >> setNameFromSiblingIfNeeded [

	(self hasUnresolved: #name)
		ifTrue: [ self name: (self baseTrait name, ' classTrait') asSymbol ]
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> sibling [

	^ self baseTrait.
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> theNonMetaClass [
	^self baseTrait
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinition >> traitTransformationString [		
		
	^ self name 
]
