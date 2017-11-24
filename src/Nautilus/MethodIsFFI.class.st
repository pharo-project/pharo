"
Custom icon for methods which uses FFI
"
Class {
	#name : #MethodIsFFI,
	#superclass : #AbstractMethodIconAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodIsFFI >> actionOrder [
	"Return the priority of this action"
	
	^0
]

{ #category : #order }
MethodIsFFI >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ icon
]

{ #category : #order }
MethodIsFFI >> ffiSelectors [

	^ #( 
		ffiCall: ffiCall:module: ffiCall:options: ffiCall:module:options: 
		nbCall: nbCall:module: nbCall:options: nbCall:module:options: 
	)
]

{ #category : #order }
MethodIsFFI >> isActionHandled [

	^ ((method literalAt: 1) class == ExternalLibraryFunction) or: [
		method sendsAnySelectorOf: self ffiSelectors
	] 
]

{ #category : #order }
MethodIsFFI >> privateActionIcon [
	"Return the icon for this action"
	^ self iconNamed: #recoverLostChanges
]
