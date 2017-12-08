Class {
	#name : #ShFAbstractChange,
	#superclass : #Object,
	#instVars : [
		'oldClass',
		'newClass'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #'checking change' }
ShFAbstractChange class >> checkChangeOf: oldDefinition with: newDefinition puttingResultIn: results [
	self new addChangesFrom:oldDefinition to:newDefinition into:results.
]

{ #category : #'checking change' }
ShFAbstractChange >> addChangesFrom:oldDefinition to:newDefinition into:results. [
	self subclassResponsibility
]

{ #category : #accessing }
ShFAbstractChange >> newClass [
	^ newClass
]

{ #category : #accessing }
ShFAbstractChange >> newClass: anObject [
	newClass := anObject
]

{ #category : #accessing }
ShFAbstractChange >> oldClass [
	^ oldClass
]

{ #category : #accessing }
ShFAbstractChange >> oldClass: anObject [
	oldClass := anObject
]
