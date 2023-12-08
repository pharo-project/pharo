"
I am the responsible of installing a class in the system.

I interact with the ShiftClassBuilder to generate the classes.
You can use me as in:

Smalltalk classInstaller make: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			category: 'My-Category' ].
		
See that I should never be referenced directly, only through the accessor 
in Smalltalk or in any class in the system. 

The block passed is used to configure the builder. Check ShiftClassBuilder to see the available messages.

I have a subclass to anonymous generate classes, without registering in the environment. 
"
Class {
	#name : 'ShiftClassInstaller',
	#superclass : 'Object',
	#instVars : [
		'oldClass',
		'builder'
	],
	#category : 'Shift-ClassBuilder-Installer',
	#package : 'Shift-ClassBuilder',
	#tag : 'Installer'
}

{ #category : 'examples' }
ShiftClassInstaller class >> example [
	<sampleInstance>
	^ Smalltalk classInstaller make: [ :aSlotClassBuilder |
		aSlotClassBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			tag: 'ATag';
			package: 'My-Package' ]
]

{ #category : 'building' }
ShiftClassInstaller class >> make: aBlock [
	^ self new make:aBlock
]

{ #category : 'building' }
ShiftClassInstaller class >> remake: aClass basedOn: aBuilder [

	self new
		installingEnvironment: aBuilder installingEnvironment;
		make: [ :anotherBuilder |
			anotherBuilder fillFor: aClass.
			anotherBuilder buildEnvironment: aBuilder buildEnvironment.
			anotherBuilder markIsInRemake.
			aBuilder propagateChangesTo: anotherBuilder ]
]

{ #category : 'building' }
ShiftClassInstaller class >> update: oldClass to: aBlock [
	^ self new
		fillFor: oldClass;
		make: aBlock
]

{ #category : 'validating' }
ShiftClassInstaller class >> validateClassName: aString [
	ShiftClassBuilder new name: aString
]

{ #category : 'accessing' }
ShiftClassInstaller >> builder [
	^ builder
]

{ #category : 'accessing' }
ShiftClassInstaller >> comment: newClass [
	builder comment ifNotNil: [ newClass comment: builder comment stamp: builder commentStamp ]
]

{ #category : 'copying' }
ShiftClassInstaller >> copyObject: oldObject to: newClass [

	| newObject |
	newObject := (newClass isVariable and: [ self oldClass isVariable ])
		             ifTrue: [
			             | createdObject |
			             createdObject := newClass basicNew: oldObject basicSize.
			             "Copying the variabls"
			             1 to: oldObject basicSize do: [ :offset | createdObject basicAt: offset put: (oldObject basicAt: offset) ].
			             createdObject ]
		             ifFalse: [ newClass basicNew ].

	"first initialize all hidden slots"
	newClass classLayout allSlots
		reject: [ :aSlot | aSlot isVisible ]
		thenDo: [ :newHiddenSlot |
			newHiddenSlot initialize: newObject.
			oldObject class slotNamed: newHiddenSlot name ifFound: [ :oldSlot | newHiddenSlot write: (oldSlot read: oldObject) to: newObject ] ].

	"the initialize all visible slots"
	newClass allSlots do: [ :newSlot |
		newSlot initialize: newObject.
		oldObject class slotNamed: newSlot name ifFound: [ :oldSlot |
			newSlot write: (oldSlot read: oldObject) to: newObject ] ].

	"Clearing the readonly-ness in the old objects so the become is able to work"
	oldObject setIsReadOnlyObject: false.

	^ newObject
]

{ #category : 'initialization' }
ShiftClassInstaller >> fillFor: aClassOrTrait [
	builder fillFor: aClassOrTrait
]

{ #category : 'building' }
ShiftClassInstaller >> fixSlotScope: newClass [
	newClass superclass ifNil: [ ^ self ].
	newClass classLayout slotScope ifEmpty: [ ^ self ].

	newClass superclass classLayout slotScope == newClass classLayout slotScope parentScope
		ifFalse: [ newClass classLayout slotScope parentScope: newClass superclass classLayout slotScope ].

	self assert: newClass superclass classLayout slotScope == newClass classLayout slotScope parentScope
]

{ #category : 'initialization' }
ShiftClassInstaller >> initialize [

	super initialize.
	builder := ShiftClassBuilder new
]

{ #category : 'building' }
ShiftClassInstaller >> installInEnvironment: newClass [
	"I only install if there is a name / non anonymous"

	builder name ifNil: [ ^ self ].

	"I only install in the environment if there is not oldClass installed."
	(self installingEnvironment hasClassNamed: builder name) ifFalse: [ self installingEnvironment at: builder name put: newClass ].

	self updateBindings: (self installingEnvironment bindingOf: builder name) of: newClass
]

{ #category : 'building' }
ShiftClassInstaller >> installSubclassInSuperclass: newClass [

	"I only install if there is a name / non anonymous"
	builder name ifNil: [ ^ self ].

	newClass superclass addSubclass: newClass
]

{ #category : 'building' }
ShiftClassInstaller >> installingEnvironment [

	^ builder installingEnvironment
]

{ #category : 'building' }
ShiftClassInstaller >> installingEnvironment: anEnvironment [

	builder installingEnvironment: anEnvironment
]

{ #category : 'building' }
ShiftClassInstaller >> make [
	| newClass |

	[
		newClass := builder build.

		self installInEnvironment: newClass.

		self installSubclassInSuperclass: newClass.

		builder builderEnhancer beforeMigratingClass: builder installer: self.

		builder builderEnhancer migrateToClass: newClass installer: self.

		builder builderEnhancer afterMigratingClass: builder installer: self.

		builder builderEnhancer propagateChangesToRelatedClasses: newClass builder: builder.
		
	] on: ShNoChangesInClass do:[
		"If there are no changes in the building, I am not building or replacing nothing"
		newClass := self oldClass.
	].

	self recategorize: newClass.
	self comment: newClass.

	self notifyChanges.

	^ newClass
]

{ #category : 'building' }
ShiftClassInstaller >> make: aBlock [

	aBlock value: builder.
	^self make
]

{ #category : 'initialization' }
ShiftClassInstaller >> makeWithBuilder: aBuilder [

	builder := aBuilder.
	^ self make
]

{ #category : 'migrating' }
ShiftClassInstaller >> migrateClassTo: newClass [
	| slotsToMigrate oldClassVariables newClassVariables|

	self oldClass ifNil:[^ self].
	self assert: newClass isNotNil.

	self oldClass superclass removeSubclass: self oldClass.

	newClass subclasses: self oldClass subclasses.

	slotsToMigrate := newClass class allSlots reject:[:e | builder builderEnhancer hasToSkipSlot: e ].
	slotsToMigrate do: [ :newSlot | 
		"the slot might need to initialize the new class"
		newSlot wantsInitialization ifTrue: [ newSlot initialize: newClass ].
		self oldClass class slotNamed: newSlot name ifFound: [ :oldSlot | newSlot write: (oldSlot read: self oldClass) to: newClass ] ].

	oldClassVariables := OrderedCollection new.
	newClassVariables := OrderedCollection new.

	self oldClass classVariables do: [ :oldVar | | newVar |
		(newClass hasClassVarNamed: oldVar key)
			ifTrue: [
				newVar := newClass classVariableNamed: oldVar key.
				newVar write: oldVar read.
				newClassVariables add: newVar.
				oldClassVariables add: oldVar. ]].

	[
		(self builder hasToMigrateInstances)
			ifTrue: [ builder builderEnhancer migrateInstancesTo: newClass installer: self ].

		{ self oldClass. builder oldMetaclass } , oldClassVariables asArray
			elementsForwardIdentityTo: { newClass. builder newMetaclass }, newClassVariables asArray.

		self fixSlotScope: newClass.
		self fixSlotScope: newClass class.

		newClass classPool rehash.
	] valueUninterruptably
]

{ #category : 'migrating' }
ShiftClassInstaller >> migrateInstancesTo: newClass [
	| oldObjects newObjects readOnlyOldObjectsFlags |
	oldObjects := self oldClass allInstances.
	oldObjects ifEmpty: [ ^ self ].

	readOnlyOldObjectsFlags := OrderedCollection new: oldObjects size.

	newObjects := oldObjects collect: [ :e |
		(readOnlyOldObjectsFlags add: e isReadOnlyObject).
		self copyObject: e to: newClass ].

	oldObjects elementsForwardIdentityTo: newObjects copyHash: true.

	newObjects with: readOnlyOldObjectsFlags do: [ :anObject :aFlag | anObject setIsReadOnlyObject: aFlag ]
]

{ #category : 'notifications' }
ShiftClassInstaller >> notifyChanges [
	builder notifyChanges
]

{ #category : 'accessing' }
ShiftClassInstaller >> oldClass [
	^ builder oldClass
]

{ #category : 'accessing' }
ShiftClassInstaller >> oldClass: anObject [
	oldClass := anObject
]

{ #category : 'notifications' }
ShiftClassInstaller >> recategorize: aClass [
	"If the package is nil we leave the class unclassified"
	builder package ifNotNil: [ :package | aClass package: package tag: builder tag ]
]

{ #category : 'building' }
ShiftClassInstaller >> updateBindings: aBinding of: newClass [
	newClass methods do: [ :e | e classBinding: aBinding ]
]
