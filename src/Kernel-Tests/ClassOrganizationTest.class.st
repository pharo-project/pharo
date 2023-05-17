"
SUnit tests for class organization
"
Class {
	#name : #ClassOrganizationTest,
	#superclass : #ProtocolTest,
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #accessing }
ClassOrganizationTest >> organization [
	^ organization
]

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

	self organization addProtocol: 'test-protocol'.

	self assert: (self organization hasProtocol: 'test-protocol')
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnder [
	"Set the base for the test"

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	self organization classify: #king under: #owl.
	self organization classify: #luz under: #owl.

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king #luz ).

	"Move a method"
	self organization classify: #luz under: #one.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).

	"Move last method"
	self organization classify: #king under: #two.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( #king ).

	"Nothing should change if the new protocol is the same than the old one"
	self organization classify: #king under: #two.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( #one #luz ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( #king )
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderUnclassified [
	"Ensure unclassified is acting as any other protocol because that was not the case in the past."

	"Set the base for the test"
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	self organization classify: #king under: Protocol unclassified.
	self organization classify: #luz under: Protocol unclassified.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).
	
	"This should do nothing."
	self organization classify: #luz under: Protocol unclassified.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).

	"Now we move a method from unclassified to another protocol."
	self organization classify: #luz under: #one.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king ).
	
	"Now we move back to unclassified."
	self organization classify: #luz under: Protocol unclassified.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz )
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithNil [
	"Set the base for the test"
	| unclassified|
	unclassified := Protocol unclassified.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	self organization classify: #king under: nil.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (class selectorsInProtocol: unclassified) hasSameElements: #( #king ).
	
	self organization classify: #luz under: #owl.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified . #owl }.
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #luz ).
	
	"Now let's test the behavior if we already have a protocol.
	The behavior should change to not change the protocol but this test will ensure that the change is intentional and not a regression."
	self organization classify: #luz under: nil.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (class selectorsInProtocol: unclassified) hasSameElements: #( #king #luz ).
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithProtocol [
	"Set the base for the test"

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	self organization classify: #king under: #owl.
	self organization classify: #luz under: (self organization protocolNamed: #owl).

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (class selectorsInProtocol: #owl) hasSameElements: #( #king #luz )
]

{ #category : #tests }
ClassOrganizationTest >> testCopyFrom [

    | newOrganization |
    "First lets check the current state of the org."
    self assertCollection: self organization protocolNames hasSameElements: #( 'empty' 'one' ).
    self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
    self assertEmpty: (class selectorsInProtocol: #empty).

    "Now lets check that the new org has the same"
    newOrganization := ClassOrganization new
                           setSubject: self organization organizedClass;
                           copyFrom: self organization;
                           yourself.

    self assertCollection: newOrganization protocolNames hasSameElements: #( 'empty' 'one' ).
    self assertCollection: (newOrganization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (newOrganization protocolNamed: #empty) methodSelectors.

    "And now lets check that updating one does not update the other."
    self organization addProtocol: 'two'.
    newOrganization classify: 'new' under: 'init'.

    self assertCollection: self organization protocolNames hasSameElements: #( 'empty' 'one' 'two' ).
    self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
    self assertEmpty: (class selectorsInProtocol: #empty).
    self assertEmpty: (class selectorsInProtocol: #two).

    self assertCollection: newOrganization protocolNames hasSameElements: #( 'empty' 'one' 'init' ).
    self assertCollection: (newOrganization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (newOrganization protocolNamed: #empty) methodSelectors.
    self assertCollection: (newOrganization protocolNamed: #init) methodSelectors hasSameElements: #( 'new' )
]

{ #category : #tests }
ClassOrganizationTest >> testHasProtocol [

	self assert: (organization hasProtocol: #one).
	self deny: (organization hasProtocol: #two).

	self assert: (organization hasProtocol: (organization protocolNamed: #one)).
	self deny: (organization hasProtocol: (self class organization protocolNamed: #tests))
]

{ #category : #tests }
ClassOrganizationTest >> testProtocolNamed [

	self assert: (self organization protocolNamed: 'empty') name equals: 'empty'.
	self should: [ self organization protocolNamed: 'non existing' ] raise: NotFound
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveNonexistentSelectorsFromProtocols [

	class compiler
		protocol: #one;
		install: 'one 1'.

	class compiler
		protocol: #one;
		install: 'oneBis 1'.

	class compiler
		protocol: #two;
		install: 'two 2'.

	organization removeEmptyProtocols.

	self assertCollection: organization protocolNames hasSameElements: #( one two ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( one oneBis ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( two ).

	"Now that we asserted the actual state is good, we can test the actual method."
	class methodDict
		removeKey: #oneBis;
		removeKey: #two.
	organization removeNonexistentSelectorsFromProtocols.

	self assertCollection: organization protocolNames hasSameElements: #( one ).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( one )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAs [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	self organization renameProtocol: #one as: #two.

	self assert: (self organization hasProtocol: #two).
	self deny: (self organization hasProtocol: #one).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'one' ).
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithExistingProtocol [

	self organization classify: 'king' under: 'two'.

	self assert: (self organization hasProtocol: #one).
	self assert: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'king' ).
	self organization renameProtocol: #one as: #two.

	self assert: (self organization hasProtocol: #two).
	self deny: (self organization hasProtocol: #one).
	self assertCollection: (class selectorsInProtocol: #two) hasSameElements: #( 'one' 'king' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithExistingProtocolWithProtocol [

	| one two |
	self organization classify: 'king' under: 'two'.

	one := self organization protocolNamed: #one.
	two := self organization protocolNamed: #two.

	self assert: (self organization hasProtocol: one).
	self assert: (self organization hasProtocol: two).
	self assertCollection: one methodSelectors hasSameElements: #( 'one' ).
	self assertCollection: two methodSelectors hasSameElements: #( 'king' ).
	self organization renameProtocol: one as: two.

	self assert: (self organization hasProtocol: two).
	self deny: (self organization hasProtocol: one).
	self assertCollection: two methodSelectors hasSameElements: #( 'one' 'king' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	self organization renameProtocol: #one as: nil.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil2 [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	self organization renameProtocol: nil as: #two.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil3 [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	self organization renameProtocol: nil as: nil.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNonExistingProtocol [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' ).

	self organization renameProtocol: #two as: #one.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (class selectorsInProtocol: #one) hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithProtocol [

	| one two |
	one := self organization protocolNamed: #one.
	self assert: (self organization hasProtocol: one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: one methodSelectors hasSameElements: #( 'one' ).
	self organization renameProtocol: one as: #two.

	two := self organization protocolNamed: #two.
	self assert: (self organization hasProtocol: two).
	self deny: (self organization hasProtocol: one).
	self assertCollection: two methodSelectors hasSameElements: #( 'one' )
]
