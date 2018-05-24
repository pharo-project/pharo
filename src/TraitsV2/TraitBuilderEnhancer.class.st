"
I am here to extend the ShiftClassBuilder when creating a Trait or TraitedClass.
My class side method #isApplicableFor: is responsible of detecting if we are creating a trait or a class using a trait.

I create the new class using the TraitedMetaclass or MetaclassForTraits.
I inject the methods from TraitedClass to the generated metaclass. 

Also I add all the slots and methods in the trait composition.
"
Class {
	#name : #TraitBuilderEnhancer,
	#superclass : #ShDefaultBuilderEnhancer,
	#instVars : [
		'builder'
	],
	#category : #'TraitsV2-Class-Builder'
}

{ #category : #testing }
TraitBuilderEnhancer class >> isApplicableFor: aBuilder [
	
	aBuilder metaSuperclass = Trait ifTrue: [ ^ true ].
	aBuilder oldClass class superclass = Trait ifTrue: [ ^ true ].
	
	aBuilder oldClass class class = TraitedMetaclass ifTrue: [ ^ true ].
	aBuilder superclass class class = TraitedMetaclass ifTrue: [ ^ true ].

	aBuilder traitComposition asTraitComposition isEmpty ifFalse: [ ^ true ].
	aBuilder classTraitComposition asTraitComposition isEmpty ifFalse: [ ^ true ].

	^ false
]

{ #category : #'class modifications' }
TraitBuilderEnhancer >> afterMethodsCompiled: aBuilder [
	builder newMetaclass rebuildMethodDictionary.
	aBuilder newClass rebuildMethodDictionary.
]

{ #category : #migrating }
TraitBuilderEnhancer >> afterMigratingClass: aBuilder installer: anInstaller [

	aBuilder newClass traitComposition addUser: aBuilder newClass.
	aBuilder newMetaclass traitComposition addUser: aBuilder newMetaclass.
	
	TraitedClass addUser: aBuilder newMetaclass.
	
	"If this is a trait I have to update the users"
	aBuilder newClass isTrait ifFalse: [ ^ self ].
	
	builder newMetaclass traitUsers do: [:e | e rebuildMethodDictionary].
	aBuilder newClass traitUsers do: [:e | e rebuildMethodDictionary].
]

{ #category : #migrating }
TraitBuilderEnhancer >> beforeMigratingClass: aBuilder installer: anInstaller [
	aBuilder oldClass ifNil: [ ^ self ].
	
	aBuilder oldClass traitComposition removeUser: aBuilder oldClass.
	aBuilder oldMetaclass traitComposition removeUser: aBuilder oldMetaclass.
	
	TraitedClass removeUser: aBuilder oldMetaclass.	
	
	"If it is a Trait we should migrate the users "
	aBuilder newClass isTrait ifFalse: [ ^ self ].	
	aBuilder newClass users: aBuilder oldClass users.
	aBuilder newMetaclass users: aBuilder oldMetaclass users.	
	
]

{ #category : #events }
TraitBuilderEnhancer >> classCreated: aBuilder [

	builder newMetaclass initializeBasicMethods.
	
	builder newClass traitComposition: self traitComposition.
	builder newMetaclass traitComposition: self classTraitComposition.

]

{ #category : #accessing }
TraitBuilderEnhancer >> classTraitComposition [
	^ builder classTraitComposition asTraitComposition
]

{ #category : #accessing }
TraitBuilderEnhancer >> classTraitCompositionOf: aBuilder [ 
	
	^ aBuilder classTraitComposition asTraitComposition
]

{ #category : #utilities }
TraitBuilderEnhancer >> classTraitCompositionOfClass: aClass [ 
	
	^ aClass class traitComposition asTraitComposition
]

{ #category : #'class modifications' }
TraitBuilderEnhancer >> configureClass: newClass superclass: superclass withLayoutType: layoutType slots: slots [
	| resultingSlots |
	self validateTraitComposition: self traitComposition ofClass: builder oldClass.

	resultingSlots := self
		eliminateDuplicates: slots , self traitComposition slots
		withSuperclassSlots: (superclass ifNil: [#()] ifNotNil:[ :x | x allSlots]).

	newClass superclass: superclass withLayoutType: layoutType slots: resultingSlots
]

{ #category : #'class modifications' }
TraitBuilderEnhancer >> configureMetaclass: newMetaclass superclass: superclass withLayoutType: aLayoutType slots: classSlots [
	| resultingSlots |
	self validateTraitComposition: self classTraitComposition ofClass: builder oldMetaclass.

	resultingSlots := self
		eliminateDuplicates: classSlots , self classTraitComposition slots
		withSuperclassSlots: superclass allSlots.

	newMetaclass superclass: superclass withLayoutType: aLayoutType slots: resultingSlots
]

{ #category : #utilities }
TraitBuilderEnhancer >> eliminateDuplicates: aSlotCollection withSuperclassSlots: superclassSlots [
	| resultingSlots |
	resultingSlots := OrderedCollection new.

	aSlotCollection
		do: [ :e | 
			((resultingSlots noneSatisfy: [ :x | e name = x name ]) and: [ superclassSlots noneSatisfy: [ :x | e name = x name ] ])
				ifTrue: [ resultingSlots add: e ] ].


	^ resultingSlots asArray
]

{ #category : #initialization }
TraitBuilderEnhancer >> fillBuilder: aBuilder from: aClass [ 
	
	(aBuilder superclass isNil and: [ aClass superclass isNil ]) 
		ifTrue: [ aBuilder metaSuperclass: aClass class superclass ].
	
	aBuilder traitComposition: aClass traitComposition.
	aBuilder classTraitComposition: aClass class traitComposition.
]

{ #category : #migrating }
TraitBuilderEnhancer >> hasToSkipSlot: aSlot [
	^ Class hasSlotNamed: aSlot name
]

{ #category : #initialization }
TraitBuilderEnhancer >> initializeBuilder: aBuilder [
	super initializeBuilder: aBuilder.

	builder := aBuilder.

	aBuilder addChangeComparer: TraitCompositionChangedDetector.
	
	aBuilder metaSuperclass = Trait
		ifTrue: [ aBuilder metaclassClass: MetaclassForTraits ]
		ifFalse: [ aBuilder metaclassClass: TraitedMetaclass ]
]

{ #category : #'class modifications' }
TraitBuilderEnhancer >> propagateChangesToRelatedClasses: newClass installer: installer [

	super propagateChangesToRelatedClasses: newClass installer: installer.

	"If the newClass is a trait I have to remake the users of it"
	newClass isTrait ifFalse: [ ^ self ].
	(builder changes noneSatisfy: 
		[ :each | each hasChangeClassSideShape or: [ each hasChangeInstanceSideShape  ] ]) 
		ifTrue: [^self].

	newClass users do: [ :each | installer remake: each instanceSide ]
]

{ #category : #accessing }
TraitBuilderEnhancer >> traitComposition [
	^ builder traitComposition asTraitComposition
]

{ #category : #accessing }
TraitBuilderEnhancer >> traitCompositionOf: aBuilder [ 
	
	^ aBuilder traitComposition asTraitComposition
]

{ #category : #utilities }
TraitBuilderEnhancer >> traitCompositionOfClass: aClass [ 
	
	^ aClass traitComposition asTraitComposition
]

{ #category : #validating }
TraitBuilderEnhancer >> validateTraitComposition: aTraitComposition ofClass: aClass [ 

	^ (aTraitComposition allTraits includes: aClass) ifTrue: [ self error: 'Cyclic Trait composition' ]
]
