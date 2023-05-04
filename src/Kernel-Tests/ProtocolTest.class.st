Class {
	#name : #ProtocolTest,
	#superclass : #TestCase,
	#instVars : [
		'organization',
		'class'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #running }
ProtocolTest >> classNameForTests [

	^ #ClassForTests
]

{ #category : #running }
ProtocolTest >> setUp [

	super setUp.
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: self classNameForTests;
			         package: 'ClassOrganizer-Tests' ].

	organization := class organization
]

{ #category : #running }
ProtocolTest >> tearDown [

	class package removeFromSystem.
	super tearDown
]
