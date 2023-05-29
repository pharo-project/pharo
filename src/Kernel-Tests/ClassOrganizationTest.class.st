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
ClassOrganizationTest >> testClassifyUnder [
	"Set the base for the test"

	self assertCollection: class protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	organization classify: #king under: #owl.
	organization classify: #luz under: #owl.

	self assertCollection: class protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king #luz ).

	"Move a method"
	organization classify: #luz under: #one.
	self assertCollection: class protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).

	"Move last method"
	organization classify: #king under: #two.
	self assertCollection: class protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( #king ).

	"Nothing should change if the new protocol is the same than the old one"
	organization classify: #king under: #two.
	self assertCollection: class protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( #king )
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderUnclassified [
	"Ensure unclassified is acting as any other protocol because that was not the case in the past."

	"Set the base for the test"
	self assertCollection: class protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	organization classify: #king under: Protocol unclassified.
	organization classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).
	
	"This should do nothing."
	organization classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).

	"Now we move a method from unclassified to another protocol."
	organization classify: #luz under: #one.

	self assertCollection: class protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king ).
	
	"Now we move back to unclassified."
	organization classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz )
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithNil [
	"Set the base for the test"
	| unclassified|
	unclassified := Protocol unclassified.
	self assertCollection: class protocolNames hasSameElements: #( #empty #one ).

	organization classify: #king under: nil.

	self assertCollection: class protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (class selectorsInProtocol: unclassified) hasSameElements: #( #king ).
	
	organization classify: #luz under: #owl.

	self assertCollection: class protocolNames hasSameElements: { #empty. #one. unclassified . #owl }.
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #luz ).
	
	"Now let's test the behavior if we already have a protocol.
	The behavior should change to not change the protocol but this test will ensure that the change is intentional and not a regression."
	organization classify: #luz under: nil.

	self assertCollection: class protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (class selectorsInProtocol: unclassified) hasSameElements: #( #king #luz ).
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithProtocol [
	"Set the base for the test"

	self assertCollection: class protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	organization classify: #king under: #owl.
	organization classify: #luz under: (class protocolNamed: #owl).

	self assertCollection: class protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king #luz )
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
