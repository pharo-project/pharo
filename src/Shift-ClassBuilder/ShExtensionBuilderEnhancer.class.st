Class {
	#name : 'ShExtensionBuilderEnhancer',
	#superclass : 'ShDefaultBuilderEnhancer',
	#instVars : [
		'builder'
	],
	#category : 'Shift-ClassBuilder-Builder',
	#package : 'Shift-ClassBuilder',
	#tag : 'Builder'
}

{ #category : 'testing' }
ShExtensionBuilderEnhancer class >> isApplicableFor: aBuilder [

	aBuilder metaSuperclass = Extension ifTrue: [ ^ true ].
	aBuilder oldClass class superclass = Extension ifTrue: [ ^ true ].

	^ false
]

{ #category : 'events' }
ShExtensionBuilderEnhancer >> classCreated: x [

	"self halt."
]

{ #category : 'events' }
ShExtensionBuilderEnhancer >> configureClass: newClass superclass: superclass withLayoutType: layoutType slots: slots [

	super configureClass: newClass superclass: superclass withLayoutType: layoutType slots: slots.
	newClass extendedClass: builder extendedClass
]

{ #category : 'events' }
ShExtensionBuilderEnhancer >> initializeBuilder: aBuilder [
	super initializeBuilder: aBuilder.

	builder := aBuilder.
]
