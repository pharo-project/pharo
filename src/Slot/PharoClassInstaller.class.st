"
I am the default class installer.
I install a new or modified class in the globals dictionary and announce this change publicly.

The default environment can be changed to any SystemDictionary.

Example:
	PharoClassInstaller example
"
Class {
	#name : #PharoClassInstaller,
	#superclass : #AbstractClassInstaller,
	#instVars : [
		'environment',
		'instanceModification',
		'methodUpdateStrategy'
	],
	#category : #'Slot-ClassBuilder'
}

{ #category : #example }
PharoClassInstaller class >> example [
	^ PharoClassInstaller make: [ :aSlotClassBuilder |
		aSlotClassBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			category: 'My-Category' ].
]

{ #category : #testing }
PharoClassInstaller class >> validateClassName: aString [
	"Validate if a string can be the name of a new class. Raise an error if not."
	
	self new builder name: aString.
]

{ #category : #notifications }
PharoClassInstaller >> basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification [

	" Copy over the trait composition "
	self copyTraitCompositionFrom: oldClass to: newClass.		
	" Copy over the method organization "
	newClass organization: oldClass organization.
	" Update the subclass links "
	oldClass superclass == newClass superclass ifFalse: [ 
		oldClass superclass removeSubclass: oldClass.
		newClass superclass addSubclass: newClass ].
	" Announce if necessary "
	classModification isPropagation ifFalse: [ 
		self systemAnnouncer classDefinitionChangedFrom: oldClass to: newClass ].
]

{ #category : #notifications }
PharoClassInstaller >> classAdded: aClass inCategory: aCategory [
	" Install the new class in the system "
	environment at: aClass name put: aClass.
	environment flushClassNameCache.
	
	" Update the system's organization "
	environment organization ifNotNil: [:org | org classify: aClass name under: aCategory].
	aClass environment: environment.

	" Inform superclass of new subclass "
	aClass superclass addSubclass: aClass.	
		
	self systemAnnouncer classAdded: aClass inCategory: aCategory
]

{ #category : #accessing }
PharoClassInstaller >> classAt: aName ifAbsent: aBlock [
	^ (environment at: aName ifAbsent: aBlock)
		ifNil: aBlock
]

{ #category : #notifications }
PharoClassInstaller >> classDefinitionChangedFrom: oldClass to: newClass by: classModification [
	
	self
		copyMethodsFrom: oldClass to: newClass using: classModification;
		basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification;
		fixClassBindings: newClass
	
]

{ #category : #migrating }
PharoClassInstaller >> copyMethodsFrom: oldClass to: newClass using: classModification [
	methodUpdateStrategy
		transform: oldClass
		to: newClass
		using: classModification methodModification
]

{ #category : #accessing }
PharoClassInstaller >> environment [
	^ environment
]

{ #category : #accessing }
PharoClassInstaller >> environment: anEnvironment [
	environment := anEnvironment
]

{ #category : #migrating }
PharoClassInstaller >> fixClassBindings: newClass [
	methodUpdateStrategy updateClassLiteralKeysIn: newClass
]

{ #category : #notifications }
PharoClassInstaller >> formatChangedFrom: oldClass to: newClass by: classModification [

	self 
		classDefinitionChangedFrom: oldClass 
		to: newClass 
		by: classModification
	
]

{ #category : #initialization }
PharoClassInstaller >> initialize [
	super initialize.
	environment := nil environment.
	methodUpdateStrategy := MethodRecompileStrategy new.
]

{ #category : #migrating }
PharoClassInstaller >> migrateClasses: old to: new using: anInstanceModification [
	instanceModification := anInstanceModification.
	old ifEmpty:  [ ^ self ].
	[
		1 to: old size do: [ :index |
			self updateClass: (old at: index) to: (new at: index)].
		old elementsForwardIdentityTo: new.
	] valueUnpreemptively
]

{ #category : #notifications }
PharoClassInstaller >> recategorize: aClass to: newCategory [
	| oldCategory |
	
	oldCategory := aClass category.
	oldCategory asSymbol == newCategory asSymbol
		ifTrue: [ ^ self ].
		
	environment organization 
		classify: aClass name 
		under: newCategory.
		
	self systemAnnouncer 
		class: aClass 
		recategorizedFrom: oldCategory 
		to: newCategory
]

{ #category : #notifications }
PharoClassInstaller >> shallowClassDefinitionChangedFrom: oldClass to: newClass by: classModification [
	" Copy over the method organization "
	" Update the superclass links "
	self 
		shallowCopyMethodsFrom: oldClass to: newClass using: classModification;
		basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification
]

{ #category : #migrating }
PharoClassInstaller >> shallowCopyMethodsFrom: oldClass to: newClass using: classModification [
	newClass methodDict: oldClass methodDict
]

{ #category : #notifications }
PharoClassInstaller >> sharedVariableOrSharedPoolChangedFrom: oldClass to: newClass by: classModification [ 

	self 
		classDefinitionChangedFrom: oldClass 
		to: newClass 
		by: classModification.
	
	"for every class var in the classModification: if this exists in oldClass, copy value over"
	classModification classVariables do: [ :each | 
		(oldClass hasClassVarNamed: each name) ifTrue: [
			each write: (oldClass classVariableNamed: each name) value  ]].
	
	"Update subclasses. No need to propagate the modification to them."
	oldClass subclasses do: [ :each |
		each superclass: newClass.
		newClass addSubclass: each.
		each classLayout slotScope ifNotEmpty: [ :scope | scope parentScope: newClass classLayout slotScope ] ].
]

{ #category : #notifications }
PharoClassInstaller >> slotsChangedFrom: oldClass to: newClass by: classModification [
	
	self
		classDefinitionChangedFrom: oldClass 
		to: newClass 
		by: classModification
]

{ #category : #notifications }
PharoClassInstaller >> superclassChangedFrom: oldClass to: newClass by: classModification [

	self 
		shallowClassDefinitionChangedFrom: oldClass
		to: newClass
		by: classModification 
]

{ #category : #private }
PharoClassInstaller >> systemAnnouncer [
	^ SystemAnnouncer uniqueInstance
]

{ #category : #migrating }
PharoClassInstaller >> updateClass: oldClass to: newClass [
	newClass updateInstancesFrom: oldClass
]
