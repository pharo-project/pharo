Class {
	#name : #RBArgumentName,
	#superclass : #Object,
	#instVars : [
		'name',
		'newName',
		'value',
		'canBeRemoved',
		'canBeRenamed'
	],
	#category : #'Refactoring-Core-Support'
}

{ #category : #'instance creation' }
RBArgumentName class >> name: aString [
	^ self new 
		name: aString
]

{ #category : #'instance creation' }
RBArgumentName class >> name: aString value: anObject [ 
	^self new 
		name: aString;
		value: anObject
]

{ #category : #accessing }
RBArgumentName >> canBeRemoved [

	^ canBeRemoved ifNil: [ canBeRemoved := true ]
]

{ #category : #accessing }
RBArgumentName >> canBeRemoved: anObject [

	canBeRemoved := anObject
]

{ #category : #accessing }
RBArgumentName >> canBeRenamed [

	^ canBeRenamed ifNil: [ canBeRenamed := true ]
]

{ #category : #accessing }
RBArgumentName >> canBeRenamed: anObject [

	canBeRenamed := anObject
]

{ #category : #testing }
RBArgumentName >> hasNewName [ 
	^ self name ~= self newName
]

{ #category : #accessing }
RBArgumentName >> name [

	^ name
]

{ #category : #accessing }
RBArgumentName >> name: anObject [

	name := anObject
]

{ #category : #accessing }
RBArgumentName >> newName [

	^ newName ifNil: [ newName := name ]
]

{ #category : #accessing }
RBArgumentName >> newName: anObject [

	newName := anObject
]

{ #category : #printing }
RBArgumentName >> printOn: aStream [ 

	aStream nextPutAll: self newName
]

{ #category : #printing }
RBArgumentName >> storeOn: aStream [
	aStream
]

{ #category : #accessing }
RBArgumentName >> value1 [

	^ value
]

{ #category : #accessing }
RBArgumentName >> value: anObject [

	value := anObject
]
