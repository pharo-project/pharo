Class {
	#name : #FluidBuilder,
	#superclass : #Object,
	#instVars : [
		'shiftClassBuilder',
		'packageName',
		'slotsToBuild',
		'tagToBuild',
		'uses',
		'nameToBuild',
		'classSlotsToBuild',
		'classTraitsToBuild'
	],
	#category : #'FluidClassBuilder-Base'
}

{ #category : #building }
FluidBuilder >> build [
	"Return a class that is not installed in the system."
	
	^ self shiftClassBuilder build 


]

{ #category : #'class side' }
FluidBuilder >> classSlots: aCollection [ 
	classSlotsToBuild := aCollection collect: [ :e | e asSlot ]
]

{ #category : #'class side' }
FluidBuilder >> classSlotsToBuild [
	^ classSlotsToBuild
]

{ #category : #'class side' }
FluidBuilder >> classTraits: aCollection [ 
	classTraitsToBuild := aCollection
]

{ #category : #'class side' }
FluidBuilder >> classTraitsToBuild [
	^ classTraitsToBuild
]

{ #category : #copying }
FluidBuilder >> fillFromClass: aClass [ 
	
	aClass slots isEmpty ifFalse: [ self slots: aClass slots].	
	aClass traitComposition isEmpty ifFalse: [ self traits: aClass traitComposition].
	self package: aClass package name.
	self tag: (aClass package classTagForClass: aClass).
	
	aClass class slots isEmpty ifFalse: [ self classSlots: aClass class slots].
	aClass class traitComposition isEmpty ifFalse: [self classTraits: aClass class traitComposition].
	
	
]

{ #category : #building }
FluidBuilder >> fillShiftClassBuilder [ 

	shiftClassBuilder := ShiftClassBuilder new.
	shiftClassBuilder	
		buildEnvironment: ShSmalltalkGlobalsEnvironment new;
		name: nameToBuild.
	
	slotsToBuild ifNotNil: [ shiftClassBuilder slots: slotsToBuild ].	
	uses ifNotNil: [ 
		shiftClassBuilder traitComposition: uses asTraitComposition ].
	
	(tagToBuild isNil or: [ tagToBuild isEmpty ])
		ifTrue: [ shiftClassBuilder category: packageName ]
		ifFalse: [ tagToBuild isEmpty 
						ifFalse: [ shiftClassBuilder category: packageName, '-', tagToBuild asString ]
						ifTrue: [ shiftClassBuilder category: packageName ]
		].
	
	classSlotsToBuild 
		ifNotNil: [:s | s isEmpty ifFalse: [ shiftClassBuilder classSlots: classSlotsToBuild ]].
	classTraitsToBuild  
		ifNotNil: [ :t | t isEmpty ifFalse: [shiftClassBuilder classTraitComposition: classTraitsToBuild ]].
]

{ #category : #building }
FluidBuilder >> install [
	"Install the class in the system environment"
	
	^ ShiftClassInstaller new makeWithBuilder: self shiftClassBuilder
]

{ #category : #'accessing - mandatory' }
FluidBuilder >> nameToBuild [
	"mandatory"
	
	^ nameToBuild
]

{ #category : #'accessing - mandatory' }
FluidBuilder >> nameToBuild: aSymbol [ 
	nameToBuild := aSymbol
]

{ #category : #API }
FluidBuilder >> package: aString [

	packageName := aString.

]

{ #category : #'accessing - mandatory' }
FluidBuilder >> packageToBuild [

	^ packageName
]

{ #category : #building }
FluidBuilder >> shiftClassBuilder [ 

	^ shiftClassBuilder ifNil: [ shiftClassBuilder := self fillShiftClassBuilder ]
]

{ #category : #API }
FluidBuilder >> slots: anArray [

	slotsToBuild := anArray collect: [ :each | each asSlot ]
]

{ #category : #accessing }
FluidBuilder >> slotsToBuild [

	^ slotsToBuild
]

{ #category : #accessing }
FluidBuilder >> tag [

	^ tagToBuild
]

{ #category : #API }
FluidBuilder >> tag: aString [

	tagToBuild := aString
]

{ #category : #accessing }
FluidBuilder >> tagToBuild [

	^ tagToBuild
]

{ #category : #API }
FluidBuilder >> trait: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]

{ #category : #API }
FluidBuilder >> traits: anArray [
	"To replace the uses: which will be deprecated"
	uses := anArray
]

{ #category : #accessing }
FluidBuilder >> traitsToBuild [

	^ uses
]

{ #category : #deprecated }
FluidBuilder >> uses: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]
