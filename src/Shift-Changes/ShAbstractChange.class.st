"
I represent a change in a class detected when compared with the old version of the class.

In the changes could be only one instance of each change. So the equals and hashcode is implemented in that way.
This is because, many different change detectors can detect the same change, but I only care once. 

My  subclasses should know:

- To announce the changes if they have to.
- If they represent a change in the instance shape, requiring a  migration of instances.
- If the change affect the subclasses.
"
Class {
	#name : #ShAbstractChange,
	#superclass : #Object,
	#instVars : [
		'builder',
		'oldClass'
	],
	#category : 'Shift-Changes'
}

{ #category : #comparing }
ShAbstractChange >> = another [
	^ self species = another species and: [ builder = another builder ]
]

{ #category : #announcing }
ShAbstractChange >> announceChanges [
	
]

{ #category : #accessing }
ShAbstractChange >> builder [
	^ builder
]

{ #category : #accessing }
ShAbstractChange >> builder: anObject [
	builder := anObject.

]

{ #category : #testing }
ShAbstractChange >> hasChangeInstanceSideShape [
	^ false.
]

{ #category : #testing }
ShAbstractChange >> hasToMigrateInstances [
	^ false.
]

{ #category : #comparing }
ShAbstractChange >> hash [
	^ builder hash.
]

{ #category : #accessing }
ShAbstractChange >> oldClass [
	^ oldClass
]

{ #category : #accessing }
ShAbstractChange >> oldClass: anObject [
	oldClass := anObject
]

{ #category : #propagating }
ShAbstractChange >> propagateToSubclasses: aBuilder [
]
