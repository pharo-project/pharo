"
I am here to extend the ShiftClassBuilder when creating a Trait or TraitedClass.
My class side method #isApplicableFor: is responsible of detecting if we are creating a trait or a class using a trait.

I create the new class using the TraitedMetaclass or MetaclassForTraits.
I inject the methods from TraitedClass to the generated metaclass. 

Also I add all the slots and methods in the trait composition.
"
Class {
	#name : 'TraitBuilderEnhancer',
	#superclass : 'ShDefaultBuilderEnhancer',
	#instVars : [
		'builder',
		'shouldUpdateSubclassesMethodDictionaries'
	],
	#category : 'Traits-Class-Builder',
	#package : 'Traits',
	#tag : 'Class-Builder'
}

{ #category : 'testing' }
TraitBuilderEnhancer class >> isApplicableFor: aBuilder [

	aBuilder metaSuperclass = Trait ifTrue: [ ^ true ].
	aBuilder oldClass class superclass = Trait ifTrue: [ ^ true ].

	aBuilder oldClass class class = TraitedMetaclass ifTrue: [ ^ true ].
	aBuilder superclass class class = TraitedMetaclass ifTrue: [ ^ true ].

	aBuilder traitComposition asTraitComposition isEmpty ifFalse: [ ^ true ].
	aBuilder classTraitComposition asTraitComposition isEmpty ifFalse: [ ^ true ].

	^ false
]

{ #category : 'class modifications' }
TraitBuilderEnhancer >> afterMethodsCompiled: aBuilder [
	"we need to remove the categories leftover after removing Traits"

	aBuilder newClass removeNonexistentSelectorsFromProtocols.
	aBuilder newClass class removeNonexistentSelectorsFromProtocols.

	(self isTraitedMetaclass: aBuilder) ifFalse: [ ^ self ].

	builder newMetaclass rebuildMethodDictionary.
	aBuilder newClass rebuildMethodDictionary
]

{ #category : 'migrating' }
TraitBuilderEnhancer >> afterMigratingClass: aBuilder installer: anInstaller [

	"In case the class to build had traits and we remove them, we need to rebuild the method dictionary of subclasses in case they have traits and need the TraitedClass methods in their method dictionary."
	shouldUpdateSubclassesMethodDictionaries ifTrue: [
		aBuilder newClass allSubclassesDo: [ :subclass |
			subclass class
				initializeBasicMethods;
				rebuildMethodDictionary.
			subclass rebuildMethodDictionary ] ].

	(self isTraitedMetaclass: aBuilder) ifFalse: [ ^ self ].

	aBuilder newClass traitComposition addUser: aBuilder newClass.
	aBuilder newMetaclass traitComposition addUser: aBuilder newMetaclass.

	TraitedClass addUser: aBuilder newMetaclass.

	"If this is a trait I have to update the users"
	aBuilder newClass isTrait ifFalse: [ ^ self ].

	builder newMetaclass traitUsers do: [ :e | e rebuildMethodDictionary ].
	aBuilder newClass traitUsers do: [ :e | e rebuildMethodDictionary ]
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> allSlotsForBuilder: aBuilder [

	| resultingSlots |
	resultingSlots := (super allSlotsForBuilder: aBuilder) asOrderedCollection.

	"We reject the slots that already have a slot in the class or a superclass"
	aBuilder traitComposition asTraitComposition slotsCopy do: [ :slot |
		(resultingSlots anySatisfy: [ :slot2 | slot name = slot2 name ]) ifFalse: [ resultingSlots add: slot ] ].

	^ resultingSlots
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> allSlotsToInstallForBuilder: aBuilder [

	| resultingSlots |
	resultingSlots := self
		                  eliminateDuplicates: aBuilder layoutDefinition slots , aBuilder traitComposition asTraitComposition slotsCopy
		                  withSuperclassSlots: (aBuilder superclass
				                   ifNil: [ #(  ) ]
				                   ifNotNil: [ :x | x allSlots ]).

	^ resultingSlots
]

{ #category : 'migrating' }
TraitBuilderEnhancer >> beforeMigratingClass: aBuilder installer: anInstaller [

	aBuilder oldClass ifNil: [ ^ self ].

	"In case a class that had traits does not have them anymore, we need to check if the subclasses also have traits to rebuild their method dictionary.
	Here, we check this info and in the post migration we will use this info to do the rebuild."
	shouldUpdateSubclassesMethodDictionaries := aBuilder oldClass class class = TraitedMetaclass and: [ (self isTraitedMetaclass: aBuilder) not ].

	aBuilder oldClass traitComposition removeUser: aBuilder oldClass.
	aBuilder oldMetaclass traitComposition removeUser: aBuilder oldMetaclass.

	TraitedClass removeUser: aBuilder oldMetaclass.

	"If it is a Trait we should migrate the users "
	aBuilder newClass isTrait ifFalse: [ ^ self ].
	aBuilder newClass users: aBuilder oldClass users.
	aBuilder newMetaclass users: aBuilder oldMetaclass users
]

{ #category : 'events' }
TraitBuilderEnhancer >> classCreated: aBuilder [

	(self isTraitedMetaclass: aBuilder) ifFalse: [ ^ self ].

	builder newMetaclass initializeBasicMethods.

	builder newClass traitComposition: self traitComposition.
	builder newMetaclass traitComposition: self classTraitComposition
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> classTraitComposition [
	^ builder classTraitComposition asTraitComposition
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> classTraitCompositionOf: aBuilder [

	^ aBuilder classTraitComposition asTraitComposition
]

{ #category : 'utilities' }
TraitBuilderEnhancer >> classTraitCompositionOfClass: aClass [

	^ aClass class traitComposition asTraitComposition
]

{ #category : 'class modifications' }
TraitBuilderEnhancer >> configureClass: newClass superclass: superclass withLayoutType: layoutType slots: slots [
	| resultingSlots |
	self validateTraitComposition: self traitComposition ofClass: builder oldClass.

	resultingSlots := self allSlotsToInstallForBuilder: builder.

	newClass superclass: superclass withLayoutType: layoutType slots: resultingSlots
]

{ #category : 'class modifications' }
TraitBuilderEnhancer >> configureMetaclass: newMetaclass superclass: superclass withLayoutType: aLayoutType slots: classSlots [
	| resultingSlots |
	self validateTraitComposition: self classTraitComposition ofClass: builder oldMetaclass.

	resultingSlots := self
		eliminateDuplicates: classSlots , self classTraitComposition slotsCopy
		withSuperclassSlots: superclass allSlots.

	newMetaclass superclass: superclass withLayoutType: aLayoutType slots: resultingSlots
]

{ #category : 'utilities' }
TraitBuilderEnhancer >> eliminateDuplicates: aSlotCollection withSuperclassSlots: superclassSlots [
	| resultingSlots |
	resultingSlots := OrderedCollection new.

	aSlotCollection
		do: [ :e |
			((resultingSlots noneSatisfy: [ :x | e name = x name ]) and: [ superclassSlots noneSatisfy: [ :x | e name = x name ] ])
				ifTrue: [ resultingSlots add: e ] ].


	^ resultingSlots asArray
]

{ #category : 'initialization' }
TraitBuilderEnhancer >> fillBuilder: aBuilder from: aClass [

	(aBuilder superclass isNil and: [ aClass superclass isNil ]) ifTrue: [ aBuilder metaSuperclass: aClass class superclass ].

	aBuilder traitComposition: aClass traitComposition.
	aBuilder classTraitComposition: aClass class basicTraitComposition
]

{ #category : 'initialization' }
TraitBuilderEnhancer >> initialize [
	super initialize.
	shouldUpdateSubclassesMethodDictionaries := false
]

{ #category : 'initialization' }
TraitBuilderEnhancer >> initializeBuilder: aBuilder [
	super initializeBuilder: aBuilder.

	builder := aBuilder.

	aBuilder addChangeComparer: TraitCompositionChangedDetector
]

{ #category : 'testing' }
TraitBuilderEnhancer >> isTraitedMetaclass: aBuilder [

	^ aBuilder metaclassClass includesBehavior: TraitedMetaclass
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> metaclassClassFor: aBuilder [

	aBuilder metaSuperclass = Trait ifTrue: [ ^ MetaclassForTraits ].

	(aBuilder traitComposition asTraitComposition isNotEmpty or: [
		 aBuilder classTraitComposition asTraitComposition isNotEmpty or: [ aBuilder superclass class class = TraitedMetaclass ] ]) ifTrue: [ ^ TraitedMetaclass ].

	^ super metaclassClassFor: aBuilder
]

{ #category : 'events' }
TraitBuilderEnhancer >> migrateInstancesTo: aClass installer: aShiftClassInstaller [

	"Traits does not have instances"
	aClass isTrait
		ifTrue: [ ^ self ].

	super migrateInstancesTo: aClass installer: aShiftClassInstaller
]

{ #category : 'class modifications' }
TraitBuilderEnhancer >> propagateChangesToRelatedClasses: newClass builder: aBuilder [

	super propagateChangesToRelatedClasses: newClass builder: aBuilder.

	"If the newClass is a trait I have to remake the users of it"
	newClass isTrait ifFalse: [ ^ self ].
	(builder changes noneSatisfy: [ :each | each hasChangeClassSideShape or: [ each hasChangeInstanceSideShape ] ]) ifTrue: [ ^ self ].

	newClass users do: [ :user | user classInstaller remake: user basedOn: aBuilder ]
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> traitComposition [
	^ builder traitComposition asTraitComposition
]

{ #category : 'accessing' }
TraitBuilderEnhancer >> traitCompositionOf: aBuilder [

	^ aBuilder traitComposition asTraitComposition
]

{ #category : 'utilities' }
TraitBuilderEnhancer >> traitCompositionOfClass: aClass [

	^ aClass traitComposition asTraitComposition
]

{ #category : 'migrating' }
TraitBuilderEnhancer >> validateRedefinition: anOldClass [
	(anOldClass isNotNil and: [ anOldClass isTrait ~= builder isTrait ])
		ifTrue: [ self error: 'A class should not be redefined as a trait. First remove it from the system.' ]
]

{ #category : 'validating' }
TraitBuilderEnhancer >> validateTraitComposition: aTraitComposition ofClass: aClass [

	^ (aTraitComposition allTraits includes: aClass) ifTrue: [ self error: 'Cyclic Trait composition' ]
]
