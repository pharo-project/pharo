"
Tests for ClyCommandGroupBuilder.
"
Class {
	#name : 'ClyCommandGroupBuilderTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'tests' }
ClyCommandGroupBuilderTest >> testBuildsOnlyConcreteSubclasses [

	| group |
	group := ClyCommandGroupBuilder
		         buildFor: ClyMockBaseCommand
		         context: nil.
	self assert: group commands size equals: 2.
	self
		assert: (group commands collect: [ :each | each class name ]) asSet
		equals: #( #ClyMockFirstCommand #ClyMockSecondCommand ) asSet
]

{ #category : 'tests' }
ClyCommandGroupBuilderTest >> testGroupIsNamedAfterCommandClass [

	| group |
	group := ClyCommandGroupBuilder
		         buildFor: ClyMockBaseCommand
		         context: nil.
	self assert: group name equals: 'ClyMockBaseCommand'
]

{ #category : 'tests' }
ClyCommandGroupBuilderTest >> testSetsContextOnCommands [

	| context group |
	context := Object new.
	group := ClyCommandGroupBuilder
		         buildFor: ClyMockBaseCommand
		         context: context.
	self assert: (group commands allSatisfy: [ :each | each context == context ])
]
