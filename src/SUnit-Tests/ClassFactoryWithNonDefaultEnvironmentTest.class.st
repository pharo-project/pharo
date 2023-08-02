"
SUnit tests for ClassFactoryWithOrganization
"
Class {
	#name : #ClassFactoryWithNonDefaultEnvironmentTest,
	#superclass : #ClassFactoryForTestCaseTest,
	#category : #'SUnit-Tests-Core'
}

{ #category : #testing }
ClassFactoryWithNonDefaultEnvironmentTest >> assertEnvironmentOf: aBehavior [
	self assert: aBehavior environment equals: self environment
]

{ #category : #accessing }
ClassFactoryWithNonDefaultEnvironmentTest >> environment [

	^ factory environment
]

{ #category : #running }
ClassFactoryWithNonDefaultEnvironmentTest >> setUp [

	super setUp.
	factory := ClassFactoryForTestCase environment: SystemDictionary new
]

{ #category : #testing }
ClassFactoryWithNonDefaultEnvironmentTest >> testClassCreationInDifferentTags [

	super testClassCreationInDifferentTags.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]

{ #category : #testing }
ClassFactoryWithNonDefaultEnvironmentTest >> testMultipleClassCreation [

	self flag: #package. "Broken because of the SystemOrganizer/PackageOrganizer mixup."
	self skip.
	super testMultipleClassCreation.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]

{ #category : #testing }
ClassFactoryWithNonDefaultEnvironmentTest >> testSingleClassCreation [

	super testSingleClassCreation.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]
