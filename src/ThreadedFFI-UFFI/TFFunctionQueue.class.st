Class {
	#name : #TFFunctionQueue,
	#superclass : #Object,
	#instVars : [
		'name',
		'isMain'
	],
	#category : #'ThreadedFFI-UFFI'
}

{ #category : #'instance creation' }
TFFunctionQueue class >> main [ 

	^ self new beMain
]

{ #category : #'instance creation' }
TFFunctionQueue class >> named: aString [

	^ self new name: aString
]

{ #category : #accessing }
TFFunctionQueue >> beMain [ 

	isMain := true.
	name := nil
]

{ #category : #accessing }
TFFunctionQueue >> handle [

	^ OSXDispatchQueue uniqueInstance ensureQueueNamed: self name
]

{ #category : #initialization }
TFFunctionQueue >> initialize [

	super initialize.
	isMain := false
]

{ #category : #testing }
TFFunctionQueue >> isMain [ 

	^ isMain
]

{ #category : #accessing }
TFFunctionQueue >> name [
	^ name
]

{ #category : #accessing }
TFFunctionQueue >> name: anObject [
	name := anObject
]
