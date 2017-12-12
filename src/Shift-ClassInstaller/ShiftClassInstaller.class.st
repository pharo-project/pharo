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
		
See that I should never be referenced directly, only through the accesor 
in Smalltalk or in any class in the system. 

The block passed is used to configure the builder. Check ShiftClassBuilder to see the available messages.

I have a subclass to anonymous generate classes, without registering in the environment. 
"
Class {
	#name : #ShiftClassInstaller,
	#superclass : #Object,
	#instVars : [
		'oldClass',
		'builder'
	],
	#category : 'Shift-ClassInstaller'
}

{ #category : #example }
ShiftClassInstaller class >> example [
	^ Smalltalk classInstaller make: [ :aSlotClassBuilder |
		aSlotClassBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			category: 'My-Category' ].
]

{ #category : #building }
ShiftClassInstaller class >> make: aBlock [
	^ self new make:aBlock
]

{ #category : #building }
ShiftClassInstaller class >> update: oldClass to: aBlock [
	^ self new 
		oldClass: oldClass;
		make:aBlock
]

{ #category : #testing }
ShiftClassInstaller class >> validateClassName: aString [ 
	ShiftClassBuilder new name: aString.
]

{ #category : #accessing }
ShiftClassInstaller >> builder [
	^ builder
]

{ #category : #accessing }
ShiftClassInstaller >> comment: newClass [
	builder comment ifNotNil: [ newClass classComment: builder comment stamp: builder commentStamp ]
]

{ #category : #copying }
ShiftClassInstaller >> copyObject: oldObject to: newClass [
	| newObject |
	
	newObject := newClass isVariable
		ifTrue: [ newClass basicNew: oldObject size ]
		ifFalse: [ newClass basicNew ].

	newClass allSlots do: [ :newSlot | oldObject class slotNamed: newSlot name ifFound: [ :oldSlot | 
			newSlot write: (oldSlot read: oldObject) to: newObject ] ].

	newClass isVariable
		ifTrue: [ 1 to: oldObject basicSize do: [ :offset | newObject basicAt: offset put: (oldObject basicAt: offset) ] ].

	^ newObject
]

{ #category : #initialization }
ShiftClassInstaller >> initialize [
	super initialize.
	builder := ShiftClassBuilder new.
	builder buildEnvironment: ShSmalltalkGlobalsEnvironment new.	
	
	
]

{ #category : #building }
ShiftClassInstaller >> installInEnvironment: newClass [
	"I only install in the environment if there is not oldClass installed."
	(builder environment includesKey: builder name) ifFalse:[
		builder environment at: builder name put: newClass
	].

	self
		updateBindings: (builder environment associationAt: builder name)
		of: newClass
]

{ #category : #building }
ShiftClassInstaller >> lookupOldClass [
	oldClass
		ifNil: [ oldClass := builder environment at: builder name ifAbsent: [ nil ] ]
]

{ #category : #building }
ShiftClassInstaller >> make: aBlock [
	| newClass |

	aBlock value: builder.

	self lookupOldClass.

	[	
		builder oldClass: oldClass.
		newClass := builder build.

		self installInEnvironment: newClass.

		newClass superclass addSubclass: newClass.
		builder builderEnhancer beforeMigratingClass: builder installer: self.

		self migrateClassTo: newClass.

		builder builderEnhancer afterMigratingClass: builder installer: self.	

		newClass subclasses do: [ :e | self remake: e ].
	] on: ShNoChangesInClass do:[
		"If there are no changes in the building, I am not building or replacing nothing"
		newClass := oldClass.
	].

	self recategorize: newClass to: builder category.
	self comment: newClass.

	self notifyChanges.

	^ newClass
]

{ #category : #migrating }
ShiftClassInstaller >> migrateClassTo: newClass [
	| slotsToMigrate |
	oldClass ifNil:[^ self].
	self assert: newClass isNotNil.
	
	oldClass 	superclass removeSubclass: oldClass.

	newClass subclasses: oldClass subclasses.

	slotsToMigrate := newClass class allSlots reject:[:e | Class hasSlotNamed: e name].
	slotsToMigrate do: [ :newSlot | oldClass class slotNamed: newSlot name ifFound: [ :oldSlot | newSlot write: (oldSlot read: oldClass) to: newClass ] ].
	
	oldClass classVarNames do: [ :aClassVar |
		(newClass hasClassVarNamed: aClassVar) ifTrue:[ newClass classVarNamed: aClassVar put: (oldClass classVarNamed: aClassVar) ].
	].
	
	[ 	
		(self builder changes anySatisfy: [ :e | e hasToMigrateInstances ]) 
			ifTrue: [self migrateInstancesTo: newClass].

		{ oldClass. builder oldMetaclass } elementsForwardIdentityTo: { newClass. builder newMetaclass }.	
	] valueUninterruptably.

]

{ #category : #migrating }
ShiftClassInstaller >> migrateInstancesTo: newClass [
	| oldObjects newObjects |
	oldObjects := oldClass allInstances.
	newObjects := oldObjects collect: [ :e | self copyObject: e to: newClass ].

	oldObjects elementsForwardIdentityTo: newObjects copyHash: true
]

{ #category : #notifications }
ShiftClassInstaller >> notifyChanges [
	builder notifyChanges
]

{ #category : #accessing }
ShiftClassInstaller >> oldClass [
	^ oldClass
]

{ #category : #accessing }
ShiftClassInstaller >> oldClass: anObject [
	oldClass := anObject
]

{ #category : #notifications }
ShiftClassInstaller >> recategorize: aClass to: newCategory [
	| oldCategory |
	oldCategory := oldClass ifNotNil:[oldClass basicCategory].
	oldCategory == newCategory asSymbol
		ifTrue: [ ^ self ].

	builder environment organization ifNotNil: [ :e | e classify: aClass name under: newCategory ].

	(oldCategory isNil or:[ oldCategory = #Unclassified])
		ifTrue: [ SystemAnnouncer uniqueInstance classAdded: aClass inCategory: newCategory ]
		ifFalse: [ SystemAnnouncer uniqueInstance class: aClass recategorizedFrom: oldCategory to: newCategory ]
]

{ #category : #building }
ShiftClassInstaller >> remake: aClass [
	self class update: aClass to: [ :anotherBuilder | 
		anotherBuilder fillFor: aClass.
		builder propagateChangesTo: anotherBuilder.
	]
]

{ #category : #building }
ShiftClassInstaller >> updateBindings: aBinding of: newClass [
	newClass methods do: [ :e | e classBinding: aBinding ]
]
