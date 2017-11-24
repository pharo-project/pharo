"
I'm a komit object representing a change in a method.  
I wrap an MC operation (addition/removal/definition).
"
Class {
	#name : #KomitMethod,
	#superclass : #KomitObject,
	#instVars : [
		'method',
		'operation'
	],
	#classInstVars : [
		'methods'
	],
	#category : #'Komitter-Models'
}

{ #category : #initialization }
KomitMethod class >> initialize [
	
	methods := Dictionary new
]

{ #category : #'instance creation' }
KomitMethod class >> method: aMethod [

	^ self new
		method: aMethod;
		yourself
]

{ #category : #comparing }
KomitMethod >> <= aKomitObject [ 

	^ self method selector <= aKomitObject method selector
]

{ #category : #testing }
KomitMethod >> isCommitable [

	^ true
]

{ #category : #testing }
KomitMethod >> isMethod [

	^ true
]

{ #category : #'*Komitter-UI' }
KomitMethod >> koDestinationText [
	^ operation koDestinationText
]

{ #category : #'*Komitter-UI' }
KomitMethod >> koSourceText [
	
	^ self operation koSourceText
]

{ #category : #accessing }
KomitMethod >> method [
	^ method
]

{ #category : #accessing }
KomitMethod >> method: anObject [
	method := anObject
]

{ #category : #accessing }
KomitMethod >> operation [
	^ operation
]

{ #category : #protocol }
KomitMethod >> operation: anOperation [

	operation := anOperation
]

{ #category : #'*Komitter-UI' }
KomitMethod >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self method selector printOn: aStream.
	aStream << ' ]'.
]

{ #category : #accessing }
KomitMethod >> selector [

	^ self method selector
]
