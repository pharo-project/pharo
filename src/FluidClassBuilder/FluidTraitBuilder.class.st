"
Similar to class `FluidClassBuilder` but for traits
"
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
	#category : #'FluidClassBuilder-Base'
}

{ #category : #accessing }
FluidTraitBuilder >> build [

	^ shiftClassBuilder build 


]

{ #category : #accessing }
FluidTraitBuilder >> fillShiftClassBuilder [

	shiftClassBuilder := ShiftClassBuilder new.
	shiftClassBuilder buildEnvironment: ShSmalltalkGlobalsEnvironment new. 
	shiftClassBuilder 
		name: name;
		superclass: nil;
		metaSuperclass: Trait;
		metaclassClass: MetaclassForTraits;
		sharedVariables: '';
		sharedPools: ''.
	slots ifNotNil: [ shiftClassBuilder slots: slots ].	
	uses ifNotNil: [ 
		shiftClassBuilder traitComposition: uses asTraitComposition.
		shiftClassBuilder classTraitComposition: uses asTraitComposition classComposition ].
	shiftClassBuilder classSlots: #().
	(tagToBuild isNil or: [ tagToBuild isEmpty ])
		ifTrue: [ shiftClassBuilder category: packageName ]
		ifFalse: [ tagToBuild isEmpty 
						ifFalse: [ shiftClassBuilder category: packageName, '-', tagToBuild asString ]
						ifTrue: [ shiftClassBuilder category: packageName ]
		].
]

{ #category : #'as yet unclassified' }
FluidTraitBuilder >> install [
	"Install the trait in the system environment"
	
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder 
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

	packageName := aString.
	self fillShiftClassBuilder
]

{ #category : #accessing }
FluidTraitBuilder >> shiftClassBuilder [ 

	^ shiftClassBuilder 
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
