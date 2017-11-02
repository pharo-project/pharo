"
I am encapsulating a class and the operations which has been applied to it.

I implement a flyweight DP
"
Class {
	#name : #KomitClass,
	#superclass : #KomitObject,
	#instVars : [
		'operations',
		'trackedClass',
		'definition',
		'methods',
		'definitions'
	],
	#classInstVars : [
		'classes'
	],
	#category : 'Komitter-Models'
}

{ #category : #initialize }
KomitClass class >> initialize [

	classes := Dictionary new.
]

{ #category : #protocol }
KomitClass class >> resetCache [

	classes removeAll
]

{ #category : #'instance creation' }
KomitClass class >> trackedClass: trackedClass [

	^ classes 
		at: trackedClass name
		ifAbsentPut: [ self new 
			trackedClass: trackedClass;
			yourself ]
]

{ #category : #'instance creation' }
KomitClass class >> trackedClass: trackedClass forExtension: aCategory [

	^ classes 
		at: trackedClass name, aCategory
		ifAbsentPut: [ self new 
			trackedClass: trackedClass;
			yourself ]
]

{ #category : #comparing }
KomitClass >> <= aKomitObject [ 

	^ self class name <= aKomitObject class name
]

{ #category : #'*Komitter-UI' }
KomitClass >> addClassDefinition: aMCRemoval [ 

	self definition: aMCRemoval
]

{ #category : #'*Komitter-UI' }
KomitClass >> addMethodDefinition: aMCAddition [

	self addOperation: aMCAddition
]

{ #category : #private }
KomitClass >> addOperation: anOperation [

	operations add: anOperation
]

{ #category : #'*Komitter-UI' }
KomitClass >> addOrganizationDefinition: aMCModification [ 
	
	self addOperation: aMCModification
]

{ #category : #accessing }
KomitClass >> definition [
	^ definition
]

{ #category : #accessing }
KomitClass >> definition: aMCRemoval [ 
	
	definition := aMCRemoval
]

{ #category : #protocol }
KomitClass >> definitions [

	^ definitions ifNil: [ definitions := (self operations
		select: [ :each | each isClassPatch ]
		thenCollect: [ :each | each koDefinition ]) sorted ]
]

{ #category : #protocol }
KomitClass >> flush [
	super flush.
	definition := nil.
	operations removeAll.
	methods ifNotNil: [ :m | m do: [ :e | e flush ] ].
	methods := nil.
	definitions ifNotNil: [ :d | d do: [ :e | e flush ] ].
	definitions := nil
]

{ #category : #initialize }
KomitClass >> initialize [

	super initialize.
	operations := Set new
]

{ #category : #testing }
KomitClass >> isKomitClass [

	^ true
]

{ #category : #'*Komitter-UI' }
KomitClass >> koDestinationText [

	^ self isDirty
		ifTrue: [ self definition koDestinationText ]
		ifFalse: [ '' ]
]

{ #category : #'*Komitter-UI' }
KomitClass >> koSourceText [

	^ self isDirty
		ifFalse: [ 'No changes' ]
		ifTrue: [ self definition koSourceText ]
]

{ #category : #protocol }
KomitClass >> methods [

	^ methods ifNil: [ methods := (self operations 
		select: [ :each | each isMethodPatch ]
		thenCollect: [ :each | each koMethod ]) sorted ]
]

{ #category : #protocol }
KomitClass >> operations [
	
	^ (definition
		ifNil: [ operations ]
		ifNotNil: [ {definition}, operations asOrderedCollection ]) asOrderedCollection
]

{ #category : #printing }
KomitClass >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self trackedClass printOn: aStream.
	aStream << ' ]'.
]

{ #category : #accessing }
KomitClass >> trackedClass [
	^ trackedClass
]

{ #category : #accessing }
KomitClass >> trackedClass: anObject [
	trackedClass := anObject
]
