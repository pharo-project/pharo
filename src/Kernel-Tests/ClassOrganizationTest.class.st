"
SUnit tests for class organization
"
Class {
	#name : #ClassOrganizationTest,
	#superclass : #TestCase,
	#instVars : [
		'organization',
		'class'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #accessing }
ClassOrganizationTest >> organization [
	^ organization
]

{ #category : #running }
ClassOrganizationTest >> runCase [
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ super runCase ]
]

{ #category : #running }
ClassOrganizationTest >> setUp [

	super setUp.

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #ClassForTests;
			         package: 'ClassOrganizer-Tests' ].

	organization := ClassOrganization forClass: class.
	organization addProtocolNamed: 'empty'.
	organization addProtocolNamed: 'one'.
	organization classify: #one under: 'one'
]

{ #category : #running }
ClassOrganizationTest >> tearDown [
	class removeFromSystem.
	super tearDown
]

{ #category : #tests }
ClassOrganizationTest >> testAddProtocolNamed [

	self organization addProtocolNamed: 'test-protocol'.

	self assert: (self organization protocolNames includes: 'test-protocol')
]

{ #category : #tests }
ClassOrganizationTest >> testMethodSelectorsInProtocol [

	| methods |
	methods := self organization methodSelectorsInProtocol: 'empty'.
	self assertEmpty: methods.

	methods := self organization methodSelectorsInProtocol: 'one'.
	self assert: methods size equals: 1.
	self assert: methods first equals: #one
]

{ #category : #tests }
ClassOrganizationTest >> testProtocolNamed [

	self assert: (self organization protocolNamed: 'empty') name equals: 'empty'.
	self should: [ self organization protocolNamed: 'non existing' ] raise: NotFound
]

{ #category : #tests }
ClassOrganizationTest >> testProtocolNames [

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one )
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveNonExistingProtocol [

	self organization removeProtocolIfEmpty: 'non-existent'
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveProtocolIfEmpty [

	self assert: self organization protocolNames size equals: 2.
	"just ignore removing of non empty categories"
	self organization removeProtocolIfEmpty: 'one'.
	self assert: self organization protocolNames size equals: 2.
	self organization removeProtocolIfEmpty: 'empty'.
	self assert: self organization protocolNames size equals: 1.
	self assert: self organization protocolNames first equals: 'one'
]
