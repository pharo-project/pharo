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
	classTraitsToBuild := aCollection asTraitComposition 
]

{ #category : #'class side' }
FluidBuilder >> classTraitsToBuild [
	^ classTraitsToBuild
]

{ #category : #copying }
FluidBuilder >> fillFromClass: aClass [ 
	
	| tag |
	aClass slots isEmpty ifFalse: [ self slots: aClass slots].	
	aClass traitComposition isEmpty ifFalse: [ self traits: aClass traitComposition].
	self package: aClass package name.

	tag := (aClass package classTagForClass: aClass).
	tag ifNotNil: [ :s | 
		s isRoot ifFalse: [self tag: (aClass package classTagForClass: aClass) name ]].
	
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
	
	uses := anArray asTraitComposition.
	classTraitsToBuild := uses classComposition
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
