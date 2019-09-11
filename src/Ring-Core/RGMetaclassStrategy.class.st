Class {
	#name : #RGMetaclassStrategy,
	#superclass : #RGClassDescriptionStrategy,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #visiting }
RGMetaclassStrategy >> acceptVisitor: aVisitor [ 

	^ aVisitor visitMetaclass: self owner
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> allClassVarNames [

	self owner instanceSide allClassVarNames 
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> baseBehavior [

	^ self baseClass
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> baseClass [

	^ self backend forBehavior baseClassFor: self owner
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> baseClass: anRGClass [

	self backend forBehavior setBaseClassFor: self owner to: anRGClass
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> bindingOf: varName [
	
	^ self baseClass bindingOf: varName
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> category [

	^ self baseClass category
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> classSide [

	^ self owner
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> classVarNames [

	^ self theNonMetaClass classVarNames
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> comment [

	^ self baseClass comment
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> defaultMetaClass [

	"will be set correctly together with baseClass"
	^ RGUnresolvedValue new default: nil.
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> definition [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: 
		[:strm |
		strm print: self.
		self owner hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: self traitComposition ].
		
		(self owner usesSpecialSlot or: [ Slot showSlotClassDefinition ])
			ifFalse: [  
						strm
							crtab;
							nextPutAll: 'instanceVariableNames: ';
							store: self owner instanceVariablesString]
			ifTrue: [ 
						strm
							crtab;
							nextPutAll: 'slots: ';
							nextPutAll: self owner slotDefinitionString]]
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> initialize [

	baseClass := RGUnresolvedValue recursive.
	
	super initialize.
	
	
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> innerBindingOf: varName [

	^self instanceSide innerBindingOf: varName
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> instanceSide [
	^ self baseClass 
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> isMeta [

	^true
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> isMetaclass [

	^true
]

{ #category : #testing }
RGMetaclassStrategy >> isMetaclassStrategy [

	^ true
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> makeResolved [

	"try to set the correct name before resolving of it"
	((self owner hasUnresolved: #name) and: [ self baseClass isRingResolved and: [ self baseClass hasResolvedName ] ]) ifTrue: [ 
		self owner pvtName: (self baseClass name, ' class') asSymbol.
		 ].
	
	super makeResolved markAsRingResolved.
	
	baseClass := self baseClass.
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> metaclass [

	"temporary" 
	^ self
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> package [

	^ self baseClass package
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> pvtAsMetaclassTrait [

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
RGMetaclassStrategy >> pvtBaseClass [

	^ baseClass value
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> pvtBaseClass: anRGClass [

	self owner environment verifyOwnership: anRGClass.

	^ baseClass := anRGClass
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#baseClass -> baseClass. 
   	}
	
	
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> referencedBehaviors [

	^ super referencedBehaviors, {self baseClass}
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> sibling [

	^ self baseClass.
]

{ #category : #testing }
RGMetaclassStrategy >> storeName [

	^ 'RGMetaclass'
]

{ #category : #'private - backend access' }
RGMetaclassStrategy >> theNonMetaClass [
	^self baseClass
]
