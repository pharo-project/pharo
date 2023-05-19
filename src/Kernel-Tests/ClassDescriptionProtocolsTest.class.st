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
ClassDescriptionProtocolsTest >> testProtocolNameOfSelector [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: (class protocolNameOfSelector: #king) equals: #titan.
	"In the future this should maybe be an error?"
	self assert: (class protocolNameOfSelector: #luz) isNil
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
ClassDescriptionProtocolsTest >> testProtocols [

	class organization addProtocol: #titan.
	class organization addProtocol: #human.
	class organization addProtocol: #witch.

	self assertCollection: class protocols hasSameElements: (#( #titan #human #witch ) collect: [ :protocolName | class organization protocolNamed: protocolName ]).

	class organization removeProtocolIfEmpty: #titan.

	self assertCollection: class protocols hasSameElements: (#( #human #witch ) collect: [ :protocolName | class organization protocolNamed: protocolName ])
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testProtocolOfSelector [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: (class protocolOfSelector: #king) identicalTo: (class organization protocolNamed: #titan).
	self assert: (class protocolOfSelector: #king) name equals: #titan.
	"In the future this should maybe be an error?"
	self assert: (class protocolOfSelector: #luz) isNil
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveFromProtocols [

	class organization classify: #amity under: #witch.
	class organization classify: #edalyn under: #witch.
	self assert: (class organization hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #amity #edalyn ).

	class removeFromProtocols: #amity.
	self assert: (class organization hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #edalyn ).

	class removeFromProtocols: #edalyn.
	self deny: (class organization hasProtocol: #witch)
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveNonexistentSelectorsFromProtocols [

	class compiler
		protocol: #human;
		install: 'luz ^ 1'.

	class compiler
		protocol: #human;
		install: 'camila ^ 1'.

	class compiler
		protocol: #titan;
		install: 'king ^ 2'.

	class removeEmptyProtocols.

	self assertCollection: class protocolNames hasSameElements: #( human titan ).
	self assertCollection: (class organization protocolNamed: #human) methodSelectors hasSameElements: #( luz camila ).
	self assertCollection: (class organization protocolNamed: #titan) methodSelectors hasSameElements: #( king ).

	"Now that we asserted the actual state is good, we can test the actual method."
	class methodDict
		removeKey: #camila;
		removeKey: #king.
	class removeNonexistentSelectorsFromProtocols.

	self assertCollection: class protocolNames hasSameElements: #( human ).
	self assertCollection: (class organization protocolNamed: #human) methodSelectors hasSameElements: #( luz )
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
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king ).

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
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king ).

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
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king ).

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
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king ).

	class removeProtocol: (class organization protocolNamed: #human).
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch ).

	"Removing a protocol with methods inside."
	class removeProtocol: (class organization protocolNamed: #titan).
	self assert: class protocolNames size equals: 1.
	self deny: (class isLocalSelector: #king)
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testSelectorsInProtocol [

	self assertEmpty: (class selectorsInProtocol: #titan).

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king )
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testSelectorsInProtocolWithRealProtocol [

	self assertEmpty: (Protocol named: #titan).

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assertCollection: (class selectorsInProtocol: (class organization protocolNamed: #titan)) hasSameElements: #( #king )
]
