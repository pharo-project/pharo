Class {
	#name : 'InspectTest',
	#superclass : 'AbstractUITestCase',
	#category : 'Tools-Tests-Base',
	#package : 'Tools-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
InspectTest >> testInspect [
	">>inspect returns the object, vs. >>inspector returns the inspector window"
	"https://github.com/pharo-project/pharo/issues/11803"

	self assert: 1 inspector class equals: 1
]

{ #category : 'tests' }
InspectTest >> testInspector [
	">>inspect returns the object, vs. >>inspector returns the inspector window"
	"https://github.com/pharo-project/pharo/issues/11803"

	self assert: 1 inspector class equals: SpWindowPresenter
]
