"
I am notified of all the events and modifications performed by the ShiftClassBuilder.

I am the point of extension to add new features to it in a modular way.
Also I can configure the builder to add more comparers or do additional things.
"
Class {
	#name : #ShDefaultBuilderEnhancer,
	#superclass : #Object,
	#category : #'Shift-ClassBuilder'
}

{ #category : #testing }
ShDefaultBuilderEnhancer class >> isApplicableFor: aBuilder [
	^ false
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> afterMethodsCompiled: builder [
]

{ #category : #migrating }
ShDefaultBuilderEnhancer >> afterMigratingClass: aBuilder installer: anInstaller [

]

{ #category : #migrating }
ShDefaultBuilderEnhancer >> beforeMigratingClass: aBuilder installer: anInstaller [

]

{ #category : #events }
ShDefaultBuilderEnhancer >> classCreated: builder [
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> compileMethodsFor: aBuilder [
	aBuilder compileMethods 
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> configureClass: newClass superclass: superclass withLayoutType: layoutType slots: slots [
	newClass superclass: superclass withLayoutType: layoutType slots: slots
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> configureMetaclass: newMetaclass superclass: superclass withLayoutType: aLayoutType slots: classSlots [
	newMetaclass superclass: superclass withLayoutType: aLayoutType slots: classSlots
]

{ #category : #initialization }
ShDefaultBuilderEnhancer >> fillBuilder: aBuilder from: aClass [ 
]

{ #category : #migrating }
ShDefaultBuilderEnhancer >> hasToSkipSlot: aSlot [
	^ Class hasSlotNamed: aSlot name
]

{ #category : #initialization }
ShDefaultBuilderEnhancer >> initializeBuilder: aBuilder [

	aBuilder addChangeComparer: ShSharedPoolChangeDetector.
	aBuilder addChangeComparer: ShSharedVariablesChangeDetector.
	aBuilder addChangeComparer: ShSlotChangeDetector.
	aBuilder addChangeComparer: ShLayoutChangeDetector.
	aBuilder addChangeComparer: ShBitLayoutChangeDetector.
	aBuilder addChangeComparer: ShClassSlotChangeDetector.
	aBuilder addChangeComparer: ShSuperclassChangedDetector.
	aBuilder addChangeComparer: ShMetaclassChangeDetector

]

{ #category : #events }
ShDefaultBuilderEnhancer >> metaclassCreated: builder [
]

{ #category : #events }
ShDefaultBuilderEnhancer >> migrateInstancesTo: aClass installer: aShiftClassInstaller [ 

	aShiftClassInstaller migrateInstancesTo: aClass
]

{ #category : #events }
ShDefaultBuilderEnhancer >> migrateToClass: aClass installer: aShiftClassInstaller [ 

	aShiftClassInstaller migrateClassTo: aClass
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> newMetaclass: builder [
	^ builder metaclassClass new
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> on: newClass declareClassVariables: sharedVariables sharing: sharedPoolsString [
	newClass
		declareClassVariables: sharedVariables;
		sharing: sharedPoolsString
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> propagateChangesToRelatedClasses: newClass installer: installer [
	
	newClass subclasses do: [ :e | installer remake: e ]

]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> validateRedefinition: anOldClass [
	"Subclasses can validate that the new definition is coherent with the old class"
]
