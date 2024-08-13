"
A MCVariableDefinition represents a variable.
"
Class {
	#name : 'MCVariableDefinition',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'instance creation' }
MCVariableDefinition class >> name: aString [
	^ self new name: aString
	
]

{ #category : 'comparing' }
MCVariableDefinition >> = other [
	^ (self species = other species)
		and: [self name = other name]
]

{ #category : 'comparing' }
MCVariableDefinition >> hash [
	^ name hash
]

{ #category : 'testing' }
MCVariableDefinition >> isClassInstanceVariable [
	^ false
]

{ #category : 'testing' }
MCVariableDefinition >> isClassVariable [
	^ false
]

{ #category : 'testing' }
MCVariableDefinition >> isInstanceVariable [
	^ false
]

{ #category : 'testing' }
MCVariableDefinition >> isOrderDependend [
	^true
]

{ #category : 'testing' }
MCVariableDefinition >> isPoolImport [
	^ false
]

{ #category : 'accessing' }
MCVariableDefinition >> name [
	^ name
]

{ #category : 'accessing' }
MCVariableDefinition >> name: aString [
	name := aString
]

{ #category : 'printing' }
MCVariableDefinition >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(; nextPutAll: self name; nextPut: $)
]
