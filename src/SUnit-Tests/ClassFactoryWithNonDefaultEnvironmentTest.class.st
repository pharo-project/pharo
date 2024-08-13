"
SUnit tests for ClassFactoryWithOrganization
"
Class {
	#name : 'ClassFactoryWithNonDefaultEnvironmentTest',
	#superclass : 'ClassFactoryForTestCaseTest',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
ClassFactoryWithNonDefaultEnvironmentTest >> assertEnvironmentOf: aBehavior [
	self assert: aBehavior environment equals: self environment
]

{ #category : 'accessing' }
ClassFactoryWithNonDefaultEnvironmentTest >> environment [

	^ factory environment
]

{ #category : 'running' }
ClassFactoryWithNonDefaultEnvironmentTest >> setUp [

	super setUp.
	factory := ClassFactoryForTestCase environment: SystemEnvironment new
]

{ #category : 'testing' }
ClassFactoryWithNonDefaultEnvironmentTest >> testClassCreationInDifferentTags [

	super testClassCreationInDifferentTags.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]

{ #category : 'testing' }
ClassFactoryWithNonDefaultEnvironmentTest >> testMultipleClassCreation [

	super testMultipleClassCreation.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]

{ #category : 'testing' }
ClassFactoryWithNonDefaultEnvironmentTest >> testSingleClassCreation [

	super testSingleClassCreation.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]
