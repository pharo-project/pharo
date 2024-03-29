"
Mock for dependency testing
"
Class {
	#name : 'MCMockDependentItem',
	#superclass : 'Object',
	#instVars : [
		'name',
		'provides',
		'requires'
	],
	#category : 'Monticello-Tests-Mocks',
	#package : 'Monticello-Tests',
	#tag : 'Mocks'
}

{ #category : 'comparing' }
MCMockDependentItem >> <= other [
	^ self name <= other name
]

{ #category : 'accessing' }
MCMockDependentItem >> name [

	^ name
]

{ #category : 'accessing' }
MCMockDependentItem >> name: aString [

	name := aString
]

{ #category : 'accessing' }
MCMockDependentItem >> provides: anArray [

	provides := anArray
]

{ #category : 'accessing' }
MCMockDependentItem >> provisions [

	^ provides ifNil: [#()]
]

{ #category : 'comparing' }
MCMockDependentItem >> requirements [

	^ requires ifNil: [#()]
]

{ #category : 'accessing' }
MCMockDependentItem >> requires: anArray [

	requires := anArray
]
