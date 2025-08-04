Class {
	#name : 'CoMockPackageOrganizer',
	#superclass : 'Object',
	#instVars : [
		'packages'
	],
	#category : 'HeuristicCompletion-Tests-Mocks',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Mocks'
}

{ #category : 'initialization' }
CoMockPackageOrganizer >> initialize [

	super initialize.
	packages := OrderedCollection new
]

{ #category : 'initialization' }
CoMockPackageOrganizer >> packageNamed: aSymbol ifAbsent: errorBlock [

	^ packages detect: [ :each | each name asSymbol = aSymbol ]
]

{ #category : 'initialization' }
CoMockPackageOrganizer >> packageNames [

	^ packages collect: [ :each | each name ]
]

{ #category : 'initialization' }
CoMockPackageOrganizer >> packages: aCol [ 

	packages := aCol 
]
