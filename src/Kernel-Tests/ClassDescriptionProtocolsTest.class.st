"
I am a test case responsible of testing ClassDescription protocol management.
"
Class {
	#name : #ClassDescriptionProtocolsTest,
	#superclass : #TestCase,
	#instVars : [
		'class'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #helpers }
ClassDescriptionProtocolsTest >> classNameForTests [

	^ #ClassForTests
]

{ #category : #running }
ClassDescriptionProtocolsTest >> setUp [

	super setUp.
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: self classNameForTests;
			         package: 'ClassOrganizer-Tests' ]
]

{ #category : #running }
ClassDescriptionProtocolsTest >> tearDown [

	class package removeFromSystem.
	super tearDown
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testProtocolNames [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.

	self assertCollection: class protocolNames hasSameElements: #( #titan #human #witch ).

	class organization removeProtocolIfEmpty: #titan.

	self assertCollection: class protocolNames hasSameElements: #( #human #witch )
]
