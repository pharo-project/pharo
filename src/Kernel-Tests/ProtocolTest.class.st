Class {
	#name : 'ProtocolTest',
	#superclass : 'TestCase',
	#instVars : [
		'class'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'helpers' }
ProtocolTest >> classNameForTests [

	^ #ClassForTests
]

{ #category : 'running' }
ProtocolTest >> setUp [

	super setUp.
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: self classNameForTests;
			         package: 'ClassOrganizer-Tests' ]
]

{ #category : 'running' }
ProtocolTest >> tearDown [

	class package removeFromSystem.
	super tearDown
]
