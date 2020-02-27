Class {
	#name : #RGMetaclassTraitV2Strategy,
	#superclass : #RGTraitV2DescriptionStrategy,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> baseBehavior [

	^ self baseClass
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> baseClass [

	^ self backend forBehavior baseClassFor: self owner
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> baseClass: anRGClass [

	self backend forBehavior setBaseClassFor: self owner to: anRGClass
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> bindingOf: varName [
	
	^ self baseClass bindingOf: varName
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> category [

	^ self baseClass category
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> classSide [

	^ self owner
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> classVarNames [

	^ self theNonMetaClass classVarNames
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> comment [

	^ self baseClass comment
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> defaultMetaClass [

	"will be set correctly together with baseClass"
	^ RGUnresolvedValue new default: nil.
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> definition [
	^String streamContents: [:stream |
		stream
			nextPutAll: self baseTrait name;
			nextPutAll: ' clasTrait';
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self owner traitCompositionString.
		(self owner slots size > 0) ifTrue: [ 
			(self owner usesSpecialSlot or: [ Slot showSlotClassDefinition ])
				ifFalse: [  
							stream
								crtab;
								nextPutAll: 'instanceVariableNames: ';
								store: self owner instanceVariablesString]
				ifTrue: [ 
							stream
								crtab;
								nextPutAll: 'slots: ';
								nextPutAll: self owner slotDefinitionString]]]
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> initialize [

	baseClass := RGUnresolvedValue recursive.
	
	super initialize.
	
	
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> instanceSide [
	^ self baseClass 
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> isMeta [

	^true
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> isMetaclass [

	^true
]

{ #category : #testing }
RGMetaclassTraitV2Strategy >> isMetaclassStrategy [

	^ true
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> makeResolved [

	"try to set the correct name before resolving of it"
	((self owner hasUnresolved: #name) and: [ self baseClass isRingResolved and: [ self baseClass hasResolvedName ] ]) ifTrue: [ 
		self owner pvtName: (self baseClass name, ' class') asSymbol.
		 ].
	
	super makeResolved.
	
	baseClass := self baseClass markAsRingResolved.
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> metaclass [

	"temporary" 
	^ self
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> package [

	^ self baseClass package
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> pvtAsMetaclassTrait [

	| newMetaclassTrait |
	
	(self owner resolvedProperties includesAnyOf: #(superclass traitComposition))
		ifTrue: [self error: 'This class cannot be converted to trait'].
		
	"TODO: check empty layout"
	newMetaclassTrait := RGMetaclassTrait unresolvedWithParent: self owner parent.
	newMetaclassTrait copyPropertiesFrom: self owner.

	(self owner hasResolvedName)
		ifTrue: [ newMetaclassTrait pvtName: ((self owner name withoutSuffix: ' class'), ' classTrait') asSymbol ].
	newMetaclassTrait pvtLocalMethods: self owner localMethods.
	
	^ newMetaclassTrait
	
	
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> pvtBaseClass [

	^ baseClass value
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> pvtBaseClass: anRGClass [

	self owner environment verifyOwnership: anRGClass.

	^ baseClass := anRGClass
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#baseClass -> baseClass. 
   	}
	
	
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> referencedBehaviors [

	^ super referencedBehaviors, {self baseClass}
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> sibling [

	^ self baseClass.
]

{ #category : #testing }
RGMetaclassTraitV2Strategy >> storeName [

	^ 'RGMetaclass'
]

{ #category : #'private - backend access' }
RGMetaclassTraitV2Strategy >> theNonMetaClass [
	^self baseClass
]
