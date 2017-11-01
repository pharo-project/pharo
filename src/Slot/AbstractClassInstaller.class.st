"
I take a ready class and install it in a given environment.
My sublcasses implement custom strategies.
"
Class {
	#name : #AbstractClassInstaller,
	#superclass : #Object,
	#instVars : [
		'builder'
	],
	#category : 'Slot-ClassBuilder'
}

{ #category : #building }
AbstractClassInstaller class >> make: aBlock [
	| builder |
	builder := self new builder.
	aBlock value: builder.
	^ builder build
]

{ #category : #accessing }
AbstractClassInstaller >> builder [
	^ builder
]

{ #category : #notifications }
AbstractClassInstaller >> classAdded: aClass inCategory: aCategory [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractClassInstaller >> classAt: aName ifAbsent: aBlock [
	self subclassResponsibility
]

{ #category : #notifications }
AbstractClassInstaller >> classDefinitionDeeplyChangedFrom: oldClass to: newClass by: classModification [
	self subclassResponsibility
]

{ #category : #notifications }
AbstractClassInstaller >> classDefinitionShallowChangedFrom: oldClass to: newClass by: classModification [
	self subclassResponsibility
]

{ #category : #traits }
AbstractClassInstaller >> copyTraitCompositionFrom: oldClass to: newClass [
(oldClass traitComposition syntacticallyEquals: newClass traitComposition )ifTrue:[^ self ].
	self 
		installTraitComposition: oldClass traitComposition copyTraitExpression 
		on: newClass.
	"We have to manually unsubcribe the newClass from its traits. Later in the class-building phase newClass becomeForward: oldClass and hence we can no longer distinguish the two. Which would leave `newClass trait users`, an IdetitySet, in an invalid state, as it's elements have been modified (the becomeForward:) without a proper rehash."
	newClass traitComposition traits do: [ :trait |
		trait removeUser: newClass ].

]

{ #category : #accessing }
AbstractClassInstaller >> environment [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractClassInstaller >> environment: anEnvironment [
	self subclassResponsibility
]

{ #category : #initialization }
AbstractClassInstaller >> initialize [
	builder := SlotClassBuilder new.
	builder installer: self
]

{ #category : #traits }
AbstractClassInstaller >> installTraitComposition: aTraitComposition on: aClass [
	self flag: 'Should probably send something else to test'.
	aClass setTraitComposition: aTraitComposition
]

{ #category : #migrating }
AbstractClassInstaller >> migrateClasses: old to: new using: anInstanceModification [
	self subclassResponsibility
]

{ #category : #notifications }
AbstractClassInstaller >> recategorize: aClass to: aCategory [
	self subclassResponsibility
]
