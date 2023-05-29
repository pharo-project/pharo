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
ClassDescriptionProtocolsTest >> testEnsureProtocol [

	| protocol |
	class organization addProtocol: #titan.
	protocol := class protocolNamed: #titan.

	self assertCollection: class protocolNames hasSameElements: { #titan }.
	self assert: (class ensureProtocol: #titan) equals: protocol.
	self assert: (class ensureProtocol: protocol) equals: protocol.
	self should: [ class ensureProtocol: (Protocol named: #notFromClass) ] raise: Error.
	
	self assert: (class ensureProtocol: nil) name equals: Protocol unclassified.
	self assertCollection: class protocolNames hasSameElements: { #titan . Protocol unclassified }.
	
	self assert: (class ensureProtocol: #human) equals: (class protocolNamed: #human).
	self assertCollection: class protocolNames hasSameElements: { #titan . #human . Protocol unclassified }.
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testExtensionProtocols [

	| extensionPackageName |
	extensionPackageName := class package name , '2'.
	class organization addProtocol: #titan.
	[
	class organization addProtocol: '*' , extensionPackageName.

	self assertCollection: class extensionProtocols hasSameElements: { (class protocolNamed: '*' , extensionPackageName) } ] ensure: [
		(extensionPackageName asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ] ]
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testHasProtocol [

	class organization addProtocol: #titan.
	self assert: (class hasProtocol: #titan).
	self deny: (class hasProtocol: #human).

	self assert: (class hasProtocol: (class protocolNamed: #titan)).
	self deny: (class hasProtocol: (Protocol named: #human))
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testHasProtocolWithRealProtocol [

	class organization addProtocol: #titan.
	self assert: (class hasProtocol: (class protocolNamed: #titan)).
	self deny: (class hasProtocol: (Protocol named: #human))
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
ClassDescriptionProtocolsTest >> testProtocolNamed [

	| titanProtocol |
	titanProtocol := class organization addProtocol: #titan.
	self assert: (class protocolNamed: #titan) identicalTo: titanProtocol.
	self should: [ class protocolNamed: #human ] raise: NotFound
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testProtocolNamedIfAbsent [

	| titanProtocol |
	titanProtocol := class organization addProtocol: #titan.
	self assert: (class protocolNamed: #titan ifAbsent: [ self fail ]) identicalTo: titanProtocol.
	self assert: [ class protocolNamed: #human ifAbsent: [ true ] ]
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
ClassDescriptionProtocolsTest >> testProtocolOfSelector [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: (class protocolOfSelector: #king) identicalTo: (class protocolNamed: #titan).
	self assert: (class protocolOfSelector: #king) name equals: #titan.
	"In the future this should maybe be an error?"
	self assert: (class protocolOfSelector: #luz) isNil
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveFromProtocols [

	class organization classify: #amity under: #witch.
	class organization classify: #edalyn under: #witch.
	self assert: (class hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #amity #edalyn ).

	class removeFromProtocols: #amity.
	self assert: (class hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #edalyn ).

	class removeFromProtocols: #edalyn.
	self deny: (class hasProtocol: #witch)
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
	self assertCollection: (class protocolNamed: #human) methodSelectors hasSameElements: #( luz camila ).
	self assertCollection: (class protocolNamed: #titan) methodSelectors hasSameElements: #( king ).

	"Now that we asserted the actual state is good, we can test the actual method."
	class methodDict
		removeKey: #camila;
		removeKey: #king.
	class removeNonexistentSelectorsFromProtocols.

	self assertCollection: class protocolNames hasSameElements: #( human ).
	self assertCollection: (class protocolNamed: #human) methodSelectors hasSameElements: #( luz )
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

	self deny: (class hasProtocol: #titan).
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
	class removeProtocolIfEmpty: (class protocolNamed: #titan).
	self assert: class protocolNames size equals: 3.

	class removeProtocolIfEmpty: (class protocolNamed: #human).
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch )
]

{ #category : #tests }
ClassDescriptionProtocolsTest >> testRemoveProtocolOnNonExistingProtocol [

	self deny: (class hasProtocol: #titan).
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

	class removeProtocol: (class protocolNamed: #human).
	self assert: class protocolNames size equals: 2.
	self assertCollection: class protocolNames hasSameElements: #( #titan #witch ).

	"Removing a protocol with methods inside."
	class removeProtocol: (class protocolNamed: #titan).
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

	self assertCollection: (class selectorsInProtocol: (class protocolNamed: #titan)) hasSameElements: #( #king )
]
