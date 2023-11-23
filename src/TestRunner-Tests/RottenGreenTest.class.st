Class {
	#name : 'RottenGreenTest',
	#superclass : 'TestCase',
	#instVars : [
		'runner'
	],
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
}

{ #category : 'tests' }
RottenGreenTest >> anotherRottenHelper [

	self assert: false
]

{ #category : 'tests' }
RottenGreenTest >> rottenHelper [

	self assert: false.
	true ifFalse: [ self anotherRottenHelper ]
]

{ #category : 'tests' }
RottenGreenTest >> testWhereAssertIsNeverExecuted [

	| emptyCollection |
	emptyCollection := OrderedCollection new.
	emptyCollection do: [ :item | self assert: false ]
]

{ #category : 'tests' }
RottenGreenTest >> testWithARottenHelpers [

	true ifFalse: [ self rottenHelper ].
	true ifFalse: [ self anotherRottenHelper ]
]
