Class {
	#name : 'CoMockPackage',
	#superclass : 'Object',
	#instVars : [
		'definedClasses',
		'name',
		'organizer'
	],
	#category : 'HeuristicCompletion-Tests-Mocks',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
CoMockPackage >> definedClasses [ 
	
	^ definedClasses ifNil: [ definedClasses := OrderedCollection new ]
]

{ #category : 'accessing' }
CoMockPackage >> definedClasses: aCollection [
	"preferrably a collection of MockClasses"
	
	definedClasses := aCollection 
]

{ #category : 'accessing' }
CoMockPackage >> name [

	^ name
]

{ #category : 'accessing' }
CoMockPackage >> name: anObject [

	name := anObject
]

{ #category : 'accessing' }
CoMockPackage >> organizer [

	^ organizer
]

{ #category : 'accessing' }
CoMockPackage >> organizer: anObject [

	organizer := anObject
]
