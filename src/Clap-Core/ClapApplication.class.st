"
I define basic boilerplate for delegating commandline behavior to the instance side, passing the arguments match.

Declare the main commandline in a class-side method, concluding the main meaning block as follows:

somethingCommandline
	<commandline>
	^ (ClapCommand withName: 'something')
		""... description, parameters, etc""
		meaning: [ :args |
			""... preliminary checks""
			(self with: args) execute ]
"
Class {
	#name : #ClapApplication,
	#superclass : #Object,
	#instVars : [
		'arguments'
	],
	#category : #'Clap-Core-Specification'
}

{ #category : #'instance creation' }
ClapApplication class >> with: arguments [
	^ self new
		setArguments: arguments;
		yourself
]

{ #category : #accessing }
ClapApplication >> context [
	^ arguments context
]

{ #category : #execution }
ClapApplication >> execute [
	self subclassResponsibility
]

{ #category : #execution }
ClapApplication >> outputStreamDo: aBlock [
	aBlock value: self context stdout
]

{ #category : #initialization }
ClapApplication >> setArguments: args [
	arguments := args
]
