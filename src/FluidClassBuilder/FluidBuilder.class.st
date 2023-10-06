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
		'package',
		'isTrait',
		'layout',
		'sharedPools',
		'sharedVariables',
		'tag',
		'traitComposition',
		'classTraitComposition',
		'classSlots',
		'slots',
		'superclass',
		'name'
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
FluidBuilder >> classSlots [
	^ classSlots
]

{ #category : 'class side' }
FluidBuilder >> classSlots: aCollection [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	classSlots := aCollection collect: [ :e | e asSlot ]
]

{ #category : 'class side' }
FluidBuilder >> classTraitComposition [
	^ classTraitComposition
]

{ #category : 'class side' }
FluidBuilder >> classTraits: aCollection [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	classTraitComposition := aCollection asTraitComposition
]

{ #category : 'copying' }
FluidBuilder >> fillClassSideFromEnvironment: anEnvironment [

	| oldClass |
	oldClass := anEnvironment at: name ifAbsent: [ ^ self ].

	self classSlots: oldClass class slots.
	self classTraits: oldClass class traitComposition
]

{ #category : 'copying' }
FluidBuilder >> fillInstanceSideFromClass: aClass [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self privateSlots: aClass slots.
	self privateTraitComposition: aClass traitComposition.
	self package: aClass package name.

	aClass packageTag ifNotNil: [ :aTag | aTag isRoot ifFalse: [ self tag: aTag name ] ].

	self superclass: aClass superclass.

	self layout: aClass classLayout class.

	self sharedVariables: (aClass classVariables collect: [ :e | e key ]).
	self sharedPools: (aClass sharedPools collect: [ :each | each name ]) asArray
]

{ #category : 'building' }
FluidBuilder >> fillShiftClassBuilder [

	shiftClassBuilder := ShiftClassBuilder new
		                     name: self name;
		                     slots: self slots;
		                     traitComposition: self traitComposition;
		                     package: self package;
		                     tag: self tag;
		                     classSlots: self classSlots;
		                     classTraitComposition: self classTraitComposition;
		                     yourself.

	isTrait
		ifTrue: [
			shiftClassBuilder
				beTrait;
				sharedVariables: #(  );
				sharedPools: '' ]
		ifFalse: [
			shiftClassBuilder
				superclass: self superclass;
				sharedVariables: self sharedVariables;
				sharedPools: self sharedPools;
				layout: self layoutClass ].

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
	slots := #(  ).
	classSlots := #(  ).

	traitComposition := TaEmptyComposition new.
	classTraitComposition := traitComposition classComposition.

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
FluidBuilder >> layoutClass [

	^ layout
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> name [
	"mandatory"

	^ name
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> name: aSymbol [
	name := aSymbol
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
FluidBuilder >> privateSlots: anArray [

	slots := anArray collect: [ :each | each asSlot ]
]

{ #category : 'API' }
FluidBuilder >> privateTraitComposition: anArray [

	traitComposition := anArray asTraitComposition
]

{ #category : 'accessing' }
FluidBuilder >> sharedPools [

	^ sharedPools
]

{ #category : 'API' }
FluidBuilder >> sharedPools: aString [

	sharedPools := aString
]

{ #category : 'accessing' }
FluidBuilder >> sharedVariables [

	^ sharedVariables
]

{ #category : 'API' }
FluidBuilder >> sharedVariables: aCollection [

	sharedVariables := aCollection
]

{ #category : 'building' }
FluidBuilder >> shiftClassBuilder [

	^ shiftClassBuilder ifNil: [ shiftClassBuilder := self fillShiftClassBuilder ]
]

{ #category : 'accessing' }
FluidBuilder >> slots [

	^ slots
]

{ #category : 'API' }
FluidBuilder >> slots: anArray [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self privateSlots: anArray
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> superclass [

	^ superclass
]

{ #category : 'accessing - mandatory' }
FluidBuilder >> superclass: aClass [

	superclass := aClass
]

{ #category : 'accessing' }
FluidBuilder >> tag [

	^ tag
]

{ #category : 'API' }
FluidBuilder >> tag: aString [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	tag := aString
]

{ #category : 'accessing' }
FluidBuilder >> traitComposition [

	^ traitComposition
]

{ #category : 'API' }
FluidBuilder >> traits: anArray [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	traitComposition := anArray asTraitComposition.
	classTraitComposition := traitComposition classComposition
]
