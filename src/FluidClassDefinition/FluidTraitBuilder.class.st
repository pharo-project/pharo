Class {
	#name : #FluidTraitBuilder,
	#superclass : #Object,
	#instVars : [
		'packageName',
		'slots',
		'name',
		'uses',
		'tagToBuild',
		'shiftClassBuilder'
	],
	#category : #'FluidClassDefinition-Builder'
}

{ #category : #accessing }
FluidTraitBuilder >> build [

	^ shiftClassBuilder build 


]

{ #category : #accessing }
FluidTraitBuilder >> fillShiftClassBuilder [

	shiftClassBuilder  := ShiftClassBuilder new.
	shiftClassBuilder  buildEnvironment: ShSmalltalkGlobalsEnvironment new. 
	shiftClassBuilder 
		name: name;
		superclass: nil;
		metaSuperclass: Trait;
		metaclassClass: MetaclassForTraits.
	tagToBuild isNil or: [ tagToBuild isEmpty 
		ifTrue: [ shiftClassBuilder category: packageName ]
		ifFalse: [ shiftClassBuilder category: packageName, '-',tagToBuild asString ]].
	slots ifNotNil: [ shiftClassBuilder slots: slots ].
	shiftClassBuilder	
		sharedVariables: '';
		sharedPools: '';
		category: packageName.
	uses ifNotNil: [ 
		shiftClassBuilder traitComposition: uses asTraitComposition.
		shiftClassBuilder classTraitComposition: uses asTraitComposition classComposition ].
	shiftClassBuilder classSlots: #()
]

{ #category : #accessing }
FluidTraitBuilder >> instanceVariables: anArray [

	slots := anArray
]

{ #category : #accessing }
FluidTraitBuilder >> name: aString [

	name := aString
]

{ #category : #accessing }
FluidTraitBuilder >> package: aString [
	self fillShiftClassBuilder. 
	packageName := aString.
	
]

{ #category : #accessing }
FluidTraitBuilder >> slots: anArray [

	slots := anArray
]

{ #category : #accessing }
FluidTraitBuilder >> tag [
	^ tagToBuild
]

{ #category : #accessing }
FluidTraitBuilder >> tag: anObject [
	tagToBuild := anObject
]

{ #category : #accessing }
FluidTraitBuilder >> uses: anArray [

	uses := anArray
]
