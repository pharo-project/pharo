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
	#name : 'FluidClassBuilder',
	#superclass : 'FluidBuilder',
	#instVars : [
		'superclassToBuild',
		'layout',
		'sharedVariables',
		'sharedPools'
	],
	#category : 'FluidClassBuilder-Base',
	#package : 'FluidClassBuilder',
	#tag : 'Base'
}

{ #category : 'copying' }
FluidClassBuilder >> fillInstanceSideFromClass: aClass [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	super fillInstanceSideFromClass: aClass.
	self superclass: aClass superclass.

	self layout: aClass classLayout class.

	self sharedVariables: (aClass classVariables collect: [ :e | e key ]).
	self sharedPools: 	(aClass sharedPools collect: [ :each | each name ]) asArray
]

{ #category : 'building' }
FluidClassBuilder >> fillShiftClassBuilder [

	super fillShiftClassBuilder.

	shiftClassBuilder
		superclass: superclassToBuild;
		sharedVariables: sharedVariables;
		sharedPoolsCollectionOfSymbols: sharedPools;
		layoutClass: layout.

	^ shiftClassBuilder
]

{ #category : 'initialization' }
FluidClassBuilder >> initialize [

	super initialize.
	sharedVariables := #().
	sharedPools := #().
	layout := FixedLayout
]

{ #category : 'API' }
FluidClassBuilder >> layout: aClass [

	layout := aClass
]

{ #category : 'accessing' }
FluidClassBuilder >> layoutToBuild [

	^ layout
]

{ #category : 'API' }
FluidClassBuilder >> sharedPools: aString [

	sharedPools := aString
]

{ #category : 'accessing' }
FluidClassBuilder >> sharedPoolsToBuild [

	^ sharedPools
]

{ #category : 'API' }
FluidClassBuilder >> sharedVariables: aCollection [

	sharedVariables := aCollection
]

{ #category : 'accessing' }
FluidClassBuilder >> sharedVariablesToBuild [

	^ sharedVariables
]

{ #category : 'accessing - mandatory' }
FluidClassBuilder >> superclass: aClass [

	superclassToBuild := aClass
]

{ #category : 'accessing - mandatory' }
FluidClassBuilder >> superclassToBuild [

	^ superclassToBuild
]

{ #category : 'accessing - mandatory' }
FluidClassBuilder >> superclassToBuild: aClass [

	superclassToBuild := aClass
]
