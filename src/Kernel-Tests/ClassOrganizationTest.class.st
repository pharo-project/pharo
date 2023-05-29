"
SUnit tests for class organization
"
Class {
	#name : #ClassOrganizationTest,
	#superclass : #ProtocolTest,
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #private }
ClassOrganizationTest >> performTest [

	SystemAnnouncer uniqueInstance suspendAllWhile: [ super performTest ]
]

{ #category : #running }
ClassOrganizationTest >> setUp [

	super setUp.

	organization addProtocol: 'empty'.
	organization addProtocol: 'one'.
	organization classify: #one under: 'one'
]

{ #category : #tests }
ClassOrganizationTest >> testAddProtocol [

	organization addProtocol: 'test-protocol'.

	self assert: (class hasProtocol: 'test-protocol')
]

{ #category : #tests }
ClassOrganizationTest >> testCopyFrom [

	| newOrganization |
	"First lets check the current state of the org."
	self assertCollection: class protocolNames hasSameElements: #( 'empty' 'one' ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	self assertEmpty: (class selectorsInProtocol: #empty).

	"Now lets check that the new org has the same"
	newOrganization := ClassOrganization new
		                   setSubject: organization organizedClass;
		                   copyFrom: organization;
		                   yourself.

	self assertCollection: newOrganization organizedClass protocolNames hasSameElements: #( 'empty' 'one' ).
	self assertCollection: (newOrganization protocols detect: [ :protocol | protocol name = #one ]) methodSelectors hasSameElements: #( 'one' ).
	self assertEmpty: (newOrganization protocols detect: [ :protocol | protocol name = #empty ]) methodSelectors.

	"And now lets check that updating one does not update the other."
	organization addProtocol: 'two'.
	newOrganization classify: 'new' under: 'init'.

	self assertCollection: class protocolNames hasSameElements: #( 'empty' 'one' 'two' ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	self assertEmpty: (class selectorsInProtocol: #empty).
	self assertEmpty: (class selectorsInProtocol: #two).

	self assertCollection: (newOrganization protocols collect: #name) hasSameElements: #( 'empty' 'one' 'init' ).
	self assertCollection: (newOrganization protocols detect: [ :protocol | protocol name = #one ]) methodSelectors hasSameElements: #( 'one' ).
	self assertEmpty: (newOrganization protocols detect: [ :protocol | protocol name = #empty ]) methodSelectors.
	self assertCollection: (newOrganization protocols detect: [ :protocol | protocol name = #init ]) methodSelectors hasSameElements: #( 'new' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAs [

	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	organization renameProtocol: #one as: #two.

	self assert: (class hasProtocol: #two).
	self deny: (class hasProtocol: #one).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'one' ).
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithExistingProtocol [

	organization classify: 'king' under: 'two'.

	self assert: (class hasProtocol: #one).
	self assert: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'king' ).
	organization renameProtocol: #one as: #two.

	self assert: (class hasProtocol: #two).
	self deny: (class hasProtocol: #one).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'one' 'king' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithExistingProtocolWithProtocol [

	| one two |
	organization classify: 'king' under: 'two'.

	one := class protocolNamed: #one.
	two := class protocolNamed: #two.

	self assert: (class hasProtocol: one).
	self assert: (class hasProtocol: two).
	self assertCollection: one methodSelectors hasSameElements: #( 'one' ).
	self assertCollection: two methodSelectors hasSameElements: #( 'king' ).
	organization renameProtocol: one as: two.

	self assert: (class hasProtocol: two).
	self deny: (class hasProtocol: one).
	self assertCollection: two methodSelectors hasSameElements: #( 'one' 'king' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil [

	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	organization renameProtocol: #one as: nil.
	"Check that nothing changed."
	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil2 [

	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	organization renameProtocol: nil as: #two.
	"Check that nothing changed."
	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil3 [

	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	organization renameProtocol: nil as: nil.
	"Check that nothing changed."
	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNonExistingProtocol [

	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	organization renameProtocol: #two as: #one.
	"Check that nothing changed."
	self assert: (class hasProtocol: #one).
	self deny: (class hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithProtocol [

	| one two |
	one := class protocolNamed: #one.
	self assert: (class hasProtocol: one).
	self deny: (class hasProtocol: #two).
	self assertCollection: one methodSelectors hasSameElements: #( 'one' ).
	organization renameProtocol: one as: #two.

	two := class protocolNamed: #two.
	self assert: (class hasProtocol: two).
	self deny: (class hasProtocol: one).
	self assertCollection: two methodSelectors hasSameElements: #( 'one' )
]
