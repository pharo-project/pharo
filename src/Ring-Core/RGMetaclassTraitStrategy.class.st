Class {
	#name : #RGMetaclassTraitStrategy,
	#superclass : #RGTraitDescriptionStrategy,
	#instVars : [
		'baseTrait'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #visiting }
RGMetaclassTraitStrategy >> acceptVisitor: aVisitor [ 

	^ aVisitor visitClassTrait: self owner
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> baseBehavior [

	^ self baseTrait
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> baseTrait [

	^ self backend forBehavior metaclassTraitBaseTraitFor: self owner
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> baseTrait: anRGClass [

	self backend forBehavior setMetaclassTraitBaseTraitFor: self to: anRGClass
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> category [

	^ self baseTrait category
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> comment [

	^ self baseTrait comment
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> definition [
	^String streamContents: [:stream |
		stream
			nextPutAll: self baseTrait name;
			nextPutAll: ' clasTrait';
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self owner traitCompositionString]
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> instanceSide [

	^ self baseTrait
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> isMeta [

	^true
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> isMetaclassTrait [

	^ true
]

{ #category : #testing }
RGMetaclassTraitStrategy >> isMetaclassTraitStrategy [

	^ true
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> makeResolved [

	
	"try to set the correct name before resolving of it"
	((self owner hasUnresolved: #name) and: [ self baseClass isRingResolved and: [ self baseTrait hasResolvedName] ]) ifTrue: [ 
		self owner pvtName: (self baseTrait name, ' classTrait') asSymbol.
		 ].

	super makeResolved.
	
	baseTrait := self baseTrait markAsRingResolved.
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> package [

	^ self baseTrait package
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> pvtBaseTrait [

	^ baseTrait value
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> pvtBaseTrait: anRGTrait [

	self owner environment verifyOwnership: anRGTrait.

	^ baseTrait := anRGTrait
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#baseTrait -> baseTrait. 
   	}
	
	
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> referencedBehaviors [

	^ super referencedBehaviors, {self baseTrait}
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> sibling [

	^ self baseTrait.
]

{ #category : #testing }
RGMetaclassTraitStrategy >> storeName [

	^ 'RGMetaclassTrait'
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> theNonMetaClass [
	^self baseTrait
]

{ #category : #'private - backend access' }
RGMetaclassTraitStrategy >> traitTransformationString [		
		
	^ self owner name 
]
