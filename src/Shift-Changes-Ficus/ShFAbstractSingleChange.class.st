Class {
	#name : #ShFAbstractSingleChange,
	#superclass : #ShFAbstractChange,
	#instVars : [
		'oldValue',
		'newValue'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #'checking change' }
ShFAbstractSingleChange >> addChangesFrom: oldDefinition to: newDefinition into: results [
	(self hasChanged: oldDefinition with: newDefinition)
		ifTrue: [ self fillFrom: oldDefinition to: newDefinition.
			results add: self ]
]

{ #category : #'checking change' }
ShFAbstractSingleChange >> fillFrom: oldDefinition to: newDefinition [
	^ self
		oldValue: (self valueOf:oldDefinition);
		newValue: (self valueOf:newDefinition);
		oldClass: oldDefinition;
		newClass: newDefinition;
		yourself
]

{ #category : #'checking change' }
ShFAbstractSingleChange >> hasChanged: oldDefinition with: newDefinition [
	^ (self valueOf: oldDefinition) ~= (self valueOf: newDefinition)
]

{ #category : #accessing }
ShFAbstractSingleChange >> newValue [
	^ newValue
]

{ #category : #accessing }
ShFAbstractSingleChange >> newValue: anObject [
	newValue := anObject
]

{ #category : #accessing }
ShFAbstractSingleChange >> oldValue [
	^ oldValue
]

{ #category : #accessing }
ShFAbstractSingleChange >> oldValue: anObject [
	oldValue := anObject
]

{ #category : #'checking change' }
ShFAbstractSingleChange >> propertyAccesor [
	^ self subclassResponsibility 
]

{ #category : #'checking change' }
ShFAbstractSingleChange >> valueOf: aDefinition [
	^ aDefinition perform: self propertyAccesor
]
