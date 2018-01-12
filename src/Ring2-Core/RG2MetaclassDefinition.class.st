"
RG2MetaclassDefinition is a concrete representation of metaclasses of classes (traits are excluded)
"
Class {
	#name : #RG2MetaclassDefinition,
	#superclass : #RG2ClassDescriptionDefinition,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> baseBehavior [

	^ self baseClass
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> baseBehavior: aBehavior [

	^ self baseClass: aBehavior
]

{ #category : #'accessing - backend' }
RG2MetaclassDefinition >> baseClass [

	^ self backend forBehavior baseClassFor: self
]

{ #category : #'accessing - backend' }
RG2MetaclassDefinition >> baseClass: aRG2ClassDefinition [

	self backend forBehavior setBaseClassFor: self to: aRG2ClassDefinition
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> bindingOf: varName [
	
	^ self baseClass bindingOf: varName
]

{ #category : #accessing }
RG2MetaclassDefinition >> category [

	^ self baseClass category
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> classVarNames [

	^ self theNonMetaClass classVarNames
]

{ #category : #accessing }
RG2MetaclassDefinition >> comment [

	^ self baseClass comment
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> defaultMetaClass [

	"will be set correctly together with baseClass"
	^ RG2UnresolvedValue new default: nil.
]

{ #category : #accessing }
RG2MetaclassDefinition >> definition [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: 
		[:strm |
		strm print: self.
		self hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: self traitComposition ].
		
		(self usesSpecialSlot or: [ Slot showSlotClassDefinition ])
			ifFalse: [  
						strm
							crtab;
							nextPutAll: 'instanceVariableNames: ';
							store: self instanceVariablesString]
			ifTrue: [ 
						strm
							crtab;
							nextPutAll: 'slots: ';
							nextPutAll: self slotDefinitionString]]
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> initialize [

	baseClass := RG2UnresolvedValue recursive.
	
	super initialize.
	
	
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> instanceSide [
	^ self baseClass 
]

{ #category : #testing }
RG2MetaclassDefinition >> isMeta [

	^true
]

{ #category : #testing }
RG2MetaclassDefinition >> isMetaclass [

	^true
]

{ #category : #resolving }
RG2MetaclassDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	"try to set the correct name before resolving of it"
	self resolveName.
	
	super makeMinimallyResolved.
	
	baseClass := self baseClass makeMinimallyResolved.
]

{ #category : #resolving }
RG2MetaclassDefinition >> makeResolved [

	"try to set the correct name before resolving of it"
	self resolveName.
	
	super makeResolved.
	
	self resolveBaseClass.
]

{ #category : #accessing }
RG2MetaclassDefinition >> package [

	^ self baseClass package
]

{ #category : #private }
RG2MetaclassDefinition >> pvtAsMetaclassTrait [

	| newMetaclassTrait |
	
	(self resolvedProperties includesAnyOf: #(superclass traitComposition))
		ifTrue: [self error: 'This class cannot be converted to trait'].
		
	"TODO: check empty layout"
	newMetaclassTrait := RG2MetaclassTraitDefinition unresolvedWithParent: self parent.
	newMetaclassTrait copyPropertiesFrom: self.

	(self hasResolved: #name)
		ifTrue: [ newMetaclassTrait pvtName: ((name withoutSuffix: ' class'), ' classTrait') asSymbol ].
	newMetaclassTrait pvtLocalMethods: localMethods.
	
	^ newMetaclassTrait
	
	
]

{ #category : #'private - backend access' }
RG2MetaclassDefinition >> pvtBaseClass [

	^ baseClass value
]

{ #category : #'private - backend access' }
RG2MetaclassDefinition >> pvtBaseClass: aaRG2ClassDefinition [

	self environment verifyOwnership: aaRG2ClassDefinition.

	^ baseClass := aaRG2ClassDefinition
]

{ #category : #'private - backend access' }
RG2MetaclassDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#baseClass -> baseClass. 
   	}
	
	
]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> referencedBehaviors [

	^ super referencedBehaviors, {self baseClass}
]

{ #category : #resolving }
RG2MetaclassDefinition >> resolveBaseClass [

	baseClass := self baseClass.
]

{ #category : #resolving }
RG2MetaclassDefinition >> resolveSibling [

	self resolveMetaclass.
	self baseClass resolveBehaviorInformation.
]

{ #category : #resolving }
RG2MetaclassDefinition >> setNameFromSiblingIfNeeded [

	(self hasUnresolved: #name)
		ifTrue: [ self name: (self baseClass name, ' class') asSymbol ]

]

{ #category : #'as yet unclassified' }
RG2MetaclassDefinition >> sibling [

	^ self baseClass.
]

{ #category : #accessing }
RG2MetaclassDefinition >> theNonMetaClass [
	^self baseClass
]
