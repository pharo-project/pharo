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

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocol [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.
	class compiler
		protocol: #titan;
		install: 'king ^1'.
	self assert: class protocolNames size equals: 3.
	self assertCollection: (class organization protocolNamed: #titan) methodSelectors hasSameElements: #( #king ).

	class removeProtocol: #human.
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch ).

	"Removing a protocol with methods inside."
	class removeProtocol: #titan.
	self assert: class protocolNames size equals: 1.
	self deny: (class isLocalSelector: #king)
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmpty [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.
	class compiler
		protocol: #titan;
		install: 'king ^1'.
	self assert: class protocolNames size equals: 3.
	self assertCollection: (class organization protocolNamed: #titan) methodSelectors hasSameElements: #( #king ).

	"just ignore removing of non empty categories"
	class removeProtocolIfEmpty: #titan.
	self assert: class protocolNames size equals: 3.

	class removeProtocolIfEmpty: #human.
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch )
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmptyOnNonExistingProtocol [

	self deny: (class organization hasProtocol: #titan).
	self shouldnt: [ class removeProtocolIfEmpty: #titan ] raise: Error
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmptyWithRealProtocol [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.
	class compiler
		protocol: #titan;
		install: 'king ^1'.
	self assert: class protocolNames size equals: 3.
	self assertCollection: (class organization protocolNamed: #titan) methodSelectors hasSameElements: #( #king ).

	"just ignore removing of non empty categories"
	class removeProtocolIfEmpty: (class organization protocolNamed: #titan).
	self assert: class protocolNames size equals: 3.

	class removeProtocolIfEmpty: (class organization protocolNamed: #human).
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch )
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolOnNonExistingProtocol [

	self deny: (class organization hasProtocol: #titan).
	self shouldnt: [ class removeProtocol: #titan ] raise: Error
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolWithRealProtocol [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.
	class compiler
		protocol: #titan;
		install: 'king ^1'.
	self assert: class protocolNames size equals: 3.
	self assertCollection: (class organization protocolNamed: #titan) methodSelectors hasSameElements: #( #king ).

	class removeProtocol: (class organization protocolNamed: #human).
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch ).

	"Removing a protocol with methods inside."
	class removeProtocol: (class organization protocolNamed: #titan).
	self assert: class protocolNames size equals: 1.
	self deny: (class isLocalSelector: #king)
]
