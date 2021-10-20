"
I'm a builder to create class definition using a fluid interface. For now I'm just wrapping a shiftClassBuilder.  In the future I should be removed and a shiftClassBuilder should be used instead.

I'm created by the message `<<` as in 

```
Object << #Point
```

My build process happens when I resolve the message `package:` as in 

```
Object << #Point 
	slots: { #x . #y };
	tag: 'Basics';
	package: 'Kernel'
```	

Now this is not totally true. This expression creates a `ShiftClassBuilder`.
And we should send `build` to the fluidclassbuilder to actually kick the building.
 
The mandatory information (name, superclass, package) are systematically passed to the builder.
Others are checked first and when available passed. 	
	
### To install a class

```
ShiftClassInstaller new makeWithBuilder: (Object << #Point33 
 slots: { #x . #y };
 tag: 'Basics';
 package: 'Kernel') shiftClassBuilder	
```

"
Class {
	#name : #FluidClassBuilder,
	#superclass : #FluidBuilder,
	#instVars : [
		'superclassToBuild',
		'layout',
		'sharedVariables',
		'traitComposition',
		'sharedPools'
	],
	#category : #'FluidClassBuilder-Base'
}

{ #category : #building }
FluidClassBuilder >> fillShiftClassBuilder [ 

	shiftClassBuilder := ShiftClassBuilder new
		buildEnvironment: ShSmalltalkGlobalsEnvironment new;
		name: nameToBuild;
		superclass: superclassToBuild.
	(tagToBuild isNil or: [ tagToBuild isEmpty ])
		ifTrue: [ shiftClassBuilder category: packageName ]
		ifFalse: [ tagToBuild isEmpty 
						ifFalse: [ shiftClassBuilder category: packageName, '-',tagToBuild asString ]
						ifTrue: [ shiftClassBuilder category: packageName ]
		].
	slotsToBuild 
		ifNotNil: [:s | s isEmpty ifFalse: [ shiftClassBuilder slots: slotsToBuild ]].
	uses 
		ifNotNil: [ :t | t isEmpty ifFalse: [shiftClassBuilder traitComposition: uses ]].
	sharedVariables 
		ifNotNil: [ :s | s isEmpty ifFalse: [shiftClassBuilder sharedVariables: sharedVariables]].
	sharedPools 
		ifNotNil: [ :p | p isEmpty ifFalse: [shiftClassBuilder sharedPoolsCollectionOfSymbols: sharedPools  ]].
	
	"I could initialize all of them to #() instead of nil but we should check also the parser"
	
	layout ifNotNil: [ shiftClassBuilder layoutClass: layout ].
	^ shiftClassBuilder
	
	


]

{ #category : #API }
FluidClassBuilder >> layout: aClass [ 
	layout := aClass
]

{ #category : #accessing }
FluidClassBuilder >> layoutToBuild [
	^ layout 
]

{ #category : #API }
FluidClassBuilder >> sharedPools: aString [

	sharedPools := aString
]

{ #category : #accessing }
FluidClassBuilder >> sharedPoolsToBuild [

	^ sharedPools
]

{ #category : #API }
FluidClassBuilder >> sharedVariables: aCollection [

	sharedVariables := aCollection
]

{ #category : #accessing }
FluidClassBuilder >> sharedVariablesToBuild [

	^ sharedVariables
]

{ #category : #'accessing - mandatory' }
FluidClassBuilder >> superclassToBuild [

	^ superclassToBuild
]

{ #category : #'accessing - mandatory' }
FluidClassBuilder >> superclassToBuild: aClass [

	superclassToBuild := aClass
]

{ #category : #API }
FluidClassBuilder >> trait: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]

{ #category : #API }
FluidClassBuilder >> traits: aTraitComposition [

	traitComposition := aTraitComposition
]

{ #category : #deprecated }
FluidClassBuilder >> uses: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]
