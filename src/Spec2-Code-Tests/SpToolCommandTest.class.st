Class {
	#name : #SpToolCommandTest,
	#superclass : #TestCase,
	#instVars : [
		'command'
	],
	#category : #'Spec2-Code-Tests'
}

{ #category : #testing }
SpToolCommandTest class >> isAbstract [

	^ self = SpToolCommandTest
]

{ #category : #accessing }
SpToolCommandTest >> commandClass [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpToolCommandTest >> newContext [
	| navigation |
	
	navigation := SpToolCommandMock new
		when: #environment: do: [ :env | "ignore" ];
		yourself.

	^ SpToolCommandMock new
		when: #environment do: [ Smalltalk globals ];
		when: #systemNavigation do: [ navigation ].
]

{ #category : #running }
SpToolCommandTest >> setUp [

	super setUp.
	command := self commandClass forContext: self newContext.

]
