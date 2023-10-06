"
I'm responsible to build class expressed with the fluid syntax.

```
```
Object << #Point 
	slots: { #x . #y };
	tag: 'Basics';
	package: 'Kernel'
```	

### Design decisions

The KEY design decisions are:

- The code snippet above should be able to be used for scripting (interactively).
- The code snippet above must return a class builder 
- A class builder should offer two important messsage: 
-- `build` that returns a class but does not install it. 
-- `install` that installs the class in the system.

To manage class side slots and traits, I follow the design of the classBuilder which is to get all the instance 
side information in addition to the class side one. 
For this the fillFromClass: method is responsible to convert all the class information into an information expected 
by the class builder. 

In the future we would like to revisit the need for such fluid class builders and also revisit the API of the class builders (shift and fluid).
There are too many conversion and we should realigned the APIs with the class default values too. 



"
Class {
	#name : 'FluidBuilder',
	#superclass : 'Object',
	#instVars : [
		'shiftClassBuilder',
		'slotsToBuild',
		'tagToBuild',
		'uses',
		'nameToBuild',
		'classSlotsToBuild',
		'classTraitsToBuild',
		'package',
		'isTrait',
		'superclassToBuild',
		'layout',
		'sharedPools',
		'sharedVariables'
	],
	#category : 'FluidClassBuilder-Base',
	#package : 'FluidClassBuilder',
	#tag : 'Base'
}

{ #category : 'testing' }
FluidBuilder class >> isAbstract [

	^ self == FluidBuilder
]

{ #category : 'API' }
FluidBuilder >> beTrait [

	isTrait := true
]

{ #category : 'building' }
FluidBuilder >> build [
	"Return a class that is not installed in the system."

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	^ self shiftClassBuilder build
]

{ #category : 'class side' }
FluidBuilder >> classSlots: aCollection [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	classSlotsToBuild := aCollection collect: [ :e | e asSlot ]
]

{ #category : 'class side' }
FluidBuilder >> classSlotsToBuild [
	^ classSlotsToBuild
]

{ #category : 'class side' }
FluidBuilder >> classTraits: aCollection [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	classTraitsToBuild := aCollection asTraitComposition
]

{ #category : 'class side' }
FluidBuilder >> classTraitsToBuild [
	^ classTraitsToBuild
]

{ #category : 'copying' }
FluidBuilder >> fillClassSideFromEnvironment: anEnvironment [

	| oldClass |
	oldClass := anEnvironment at: nameToBuild ifAbsent: [ ^ self ].

	classSlotsToBuild := oldClass class slots.
	classTraitsToBuild := oldClass class traitComposition
]

{ #category : 'copying' }
FluidBuilder >> fillInstanceSideFromClass: aClass [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	slotsToBuild := aClass slots.
	uses := aClass traitComposition.
	self package: aClass package name.

	aClass packageTag ifNotNil: [ :tag | tag isRoot ifFalse: [ self tag: tag name ] ].

	self superclass: aClass superclass.

	self layout: aClass classLayout class.

	self sharedVariables: (aClass classVariables collect: [ :e | e key ]).
	self sharedPools: (aClass sharedPools collect: [ :each | each name ]) asArray
]

{ #category : 'building' }
FluidBuilder >> fillShiftClassBuilder [

	shiftClassBuilder := ShiftClassBuilder new
		                     name: nameToBuild;
		                     slots: slotsToBuild;
		                     traitComposition: uses;
		                     package: package;
		                     tag: tagToBuild;
		                     classSlots: classSlotsToBuild;
		                     classTraitComposition: classTraitsToBuild;
		                     yourself.

	isTrait
		ifTrue: [
			shiftClassBuilder
				beTrait;
				sharedVariables: #(  );
				sharedPools: '' ]
		ifFalse: [
			shiftClassBuilder
				superclass: superclassToBuild;
				sharedVariables: sharedVariables;
				sharedPoolsCollectionOfSymbols: sharedPools;
				layoutClass: layout ].

	^ shiftClassBuilder
]

{ #category : 'building' }
FluidBuilder >> fluidInstall [
	"Install the class in the system environment, method #fluidInstall is implemented in behavior, too"

	^ self install
]

{ #category : 'initialization' }
FluidBuilder >> initialize [

	super initialize.
	slotsToBuild := #(  ).
	classSlotsToBuild := #(  ).

	uses := TaEmptyComposition new.
	classTraitsToBuild := uses classComposition.

	isTrait := false.

	sharedVariables := #(  ).
	sharedPools := #(  ).
	layout := FixedLayout
]

{ #category : 'building' }
FluidBuilder >> install [
	"Install the class in the system environment"

	^ ShiftClassInstaller new makeWithBuilder: self shiftClassBuilder
]

{ #category : 'API' }
FluidBuilder >> layout: aClass [

	layout := aClass
]

{ #category : 'accessing' }
FluidBuilder >> layoutToBuild [

	^ layout
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> nameToBuild [
	"mandatory"

	^ nameToBuild
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> nameToBuild: aSymbol [
	nameToBuild := aSymbol
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> package [

	^ package
]

{ #category : 'API' }
FluidBuilder >> package: aString [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	package := aString
]

{ #category : 'API' }
FluidBuilder >> sharedPools: aString [

	sharedPools := aString
]

{ #category : 'accessing' }
FluidBuilder >> sharedPoolsToBuild [

	^ sharedPools
]

{ #category : 'API' }
FluidBuilder >> sharedVariables: aCollection [

	sharedVariables := aCollection
]

{ #category : 'accessing' }
FluidBuilder >> sharedVariablesToBuild [

	^ sharedVariables
]

{ #category : 'building' }
FluidBuilder >> shiftClassBuilder [

	^ shiftClassBuilder ifNil: [ shiftClassBuilder := self fillShiftClassBuilder ]
]

{ #category : 'API' }
FluidBuilder >> slots: anArray [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	slotsToBuild := anArray collect: [ :each | each asSlot ]
]

{ #category : 'accessing' }
FluidBuilder >> slotsToBuild [

	^ slotsToBuild
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> superclass: aClass [

	superclassToBuild := aClass
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> superclassToBuild [

	^ superclassToBuild
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> superclassToBuild: aClass [

	superclassToBuild := aClass
]

{ #category : 'accessing' }
FluidBuilder >> tag [

	^ tagToBuild
]

{ #category : 'API' }
FluidBuilder >> tag: aString [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	tagToBuild := aString
]

{ #category : 'accessing' }
FluidBuilder >> tagToBuild [

	^ tagToBuild
]

{ #category : 'API' }
FluidBuilder >> trait: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg'
						-> '`@receiver traits: `@arg'.

	^ self traits: aTraitComposition
]

{ #category : 'API' }
FluidBuilder >> traits: anArray [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	uses := anArray asTraitComposition.
	classTraitsToBuild := uses classComposition
]

{ #category : 'accessing' }
FluidBuilder >> traitsToBuild [

	^ uses
]

{ #category : 'deprecated' }
FluidBuilder >> uses: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg'
						-> '`@receiver traits: `@arg'.

	^ self traits: aTraitComposition
]
