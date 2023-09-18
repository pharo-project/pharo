"
i used to hold shapes built by path builder in cairo backend
"
Class {
	#name : #AthensCairoPath,
	#superclass : #FFIExternalObject,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#category : #'Athens-Cairo-Paths'
}

{ #category : #'instance creation' }
AthensCairoPath class >> finalizeResourceData: aHandle [

	self primDestroyPath: aHandle
]

{ #category : #'instance creation' }
AthensCairoPath class >> primDestroyPath: aHandle [
	^ self ffiCall: #(void cairo_path_destroy ( void* aHandle ))
]

{ #category : #converting }
AthensCairoPath >> asAthensShapeOn: anAthensCanvas [

	^ self
]

{ #category : #initialization }
AthensCairoPath >> initialize [
	handle value = 0 ifTrue: [
		self error: 'Error creating new path' ].
	FFIExternalResourceManager addResource: self data: handle
]

{ #category : #drawing }
AthensCairoPath >> paintFillsUsing: aPaint on: anAthensCanvas [

	"This method is a part of rendering dispatch  Canvas->receiver->paint"

	^ aPaint athensFillPath: self on: anAthensCanvas
]

{ #category : #'instance creation' }
AthensCairoPath >> primDestroyPath: aHandle [
	^ self ffiCall: #(void cairo_path_destroy ( ulong aHandle ))
]
