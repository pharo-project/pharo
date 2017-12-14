"
I am notified of all the events and modifications performed by the ShiftClassBuilder.

I am the point of extension to add new features to it in a modular way.
Also I can configure the builder to add more comparers or do additional things.
"
Class {
	#name : #ShDefaultBuilderEnhancer,
	#superclass : #Object,
	#category : 'Shift-ClassBuilder'
}

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
	newMetaclass superclass: superclass withLayoutType: aLayoutType slots: classSlots.
]

{ #category : #initialization }
ShDefaultBuilderEnhancer >> initializeBuilder: aBuilder [
]

{ #category : #events }
ShDefaultBuilderEnhancer >> metaclassCreated: builder [
]

{ #category : #'class modifications' }
ShDefaultBuilderEnhancer >> on: newClass declareClassVariables: sharedVariables sharing: sharedPoolsString [
	newClass
		declareClassVariables: sharedVariables;
		sharing: sharedPoolsString
]
