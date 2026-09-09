"
This class is used to test refactoring changes.
"
Class {
	#name : 'ReChangeMock',
	#superclass : 'ReChange',
	#instVars : [
		'instVar'
	],
	#classVars : [
		'SharedVar'
	],
	#classInstVars : [
		'classInstVar'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'accessing' }
ReChangeMock class >> classInsVar [

	^ classInstVar
]

{ #category : 'accessing' }
ReChangeMock >> accessAll [ 
	instVar := 0. 
	SharedVar := 0.
	^ instVar + SharedVar 
]

{ #category : 'private' }
ReChangeMock >> executeNotifying: aBlock [
	"I'm a mock, I do nothing"
	^ self
]

{ #category : 'initialization' }
ReChangeMock >> initialize [

	<ignoreUnusedVariables: #( #instVar )>
	super initialize
]

{ #category : 'accessing' }
ReChangeMock >> one [

	^ 1
]

{ #category : 'printing' }
ReChangeMock >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' name: ';
		print: name
]

{ #category : 'accessing' }
ReChangeMock >> renameChangesForClass: oldClassName to: newClassName [
	"I'm a mock, I do nothing"
	^ self
]
