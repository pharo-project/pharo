"
A mock of context for testing commands
"
Class {
	#name : 'SycMockMethodContext',
	#superclass : 'Object',
	#instVars : [
		'selectedMethods'
	],
	#category : 'SystemCommands-MethodCommands-Tests-TestData',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'TestData'
}

{ #category : 'testing' }
SycMockMethodContext >> isClassSideMethodSelected [

	^ self lastSelectedMessage notNil and: [ self lastSelectedMessage methodClass isMeta ]
]

{ #category : 'testing' }
SycMockMethodContext >> isExtensionMethodSelected [

	^ self isMethodSelected and: [ self selectedMethods anySatisfy: [ :method | method isExtension ] ]
]

{ #category : 'testing' }
SycMockMethodContext >> isInstanceSideMethodSelected [

	^ self lastSelectedMessage notNil and: [ self lastSelectedMessage methodClass isMeta not ]
]

{ #category : 'testing' }
SycMockMethodContext >> isMessageSelected [

	^ false
]

{ #category : 'testing' }
SycMockMethodContext >> isMethodSelected [

	^ self selectedMethods isNotEmpty
]

{ #category : 'accessing' }
SycMockMethodContext >> lastSelectedClass [
	^ selectedMethods last methodClass
]

{ #category : 'accessing' }
SycMockMethodContext >> lastSelectedMessage [

	^ self selectedMethods ifNotNil: [ :methods | methods ifEmpty: [ nil ] ifNotEmpty: [ methods first ] ]
]

{ #category : 'accessing' }
SycMockMethodContext >> selectedMethods [
	^ selectedMethods ifNil: [ #() ]
]

{ #category : 'accessing' }
SycMockMethodContext >> selectedMethods: aCollection [
	selectedMethods := aCollection
]
