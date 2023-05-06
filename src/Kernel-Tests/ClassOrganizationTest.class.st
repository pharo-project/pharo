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
	self assertCollection: (self organization protocolNamed: #owl) methodSelectors hasSameElements: #( #king #luz ).

	"Move a method"
	self organization classify: #luz under: #one.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (self organization protocolNamed: #owl) methodSelectors hasSameElements: #( #king ).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( #one #luz ).

	"Move last method"
	self organization classify: #king under: #two.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( #one #luz ).
	self assertCollection: (self organization protocolNamed: #two) methodSelectors hasSameElements: #( #king ).

	"Nothing should change if the new protocol is the same than the old one"
	self organization classify: #king under: #two.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #two ).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( #one #luz ).
	self assertCollection: (self organization protocolNamed: #two) methodSelectors hasSameElements: #( #king )
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
	self assertCollection: (self organization protocolNamed: Protocol unclassified) methodSelectors hasSameElements: #( #king #luz ).
	
	"This should do nothing."
	self organization classify: #luz under: Protocol unclassified.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: (self organization protocolNamed: Protocol unclassified) methodSelectors hasSameElements: #( #king #luz ).

	"Now we move a method from unclassified to another protocol."
	self organization classify: #luz under: #one.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: (self organization protocolNamed: Protocol unclassified) methodSelectors hasSameElements: #( #king ).
	
	"Now we move back to unclassified."
	self organization classify: #luz under: Protocol unclassified.

	self assertCollection: self organization protocolNames hasSameElements: {#empty. #one. Protocol unclassified }.
	self assertCollection: (self organization protocolNamed: Protocol unclassified) methodSelectors hasSameElements: #( #king #luz )
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithNil [
	"Set the base for the test"
	| unclassified|
	unclassified := Protocol unclassified.
	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	self organization classify: #king under: nil.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (self organization protocolNamed: unclassified ) methodSelectors hasSameElements: #( #king ).
	
	self organization classify: #luz under: #owl.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified . #owl }.
	self assertCollection: (self organization protocolNamed: #owl ) methodSelectors hasSameElements: #( #luz ).
	
	"Now let's test the behavior if we already have a protocol.
	The behavior should change to not change the protocol but this test will ensure that the change is intentional and not a regression."
	self organization classify: #luz under: nil.

	self assertCollection: self organization protocolNames hasSameElements: { #empty. #one. unclassified }.
	self assertCollection: (self organization protocolNamed: unclassified ) methodSelectors hasSameElements: #( #king #luz ).
]

{ #category : #tests }
ClassOrganizationTest >> testClassifyUnderWithProtocol [
	"Set the base for the test"

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one ).

	"Lets create a new protocol via classification"
	self organization classify: #king under: #owl.
	self organization classify: #luz under: (self organization protocolNamed: #owl).

	self assertCollection: self organization protocolNames hasSameElements: #( #empty #one #owl ).
	self assertCollection: (self organization protocolNamed: #owl) methodSelectors hasSameElements: #( #king #luz )
]

{ #category : #tests }
ClassOrganizationTest >> testCopyFrom [

    | newOrganization |
    "First lets check the current state of the org."
    self assertCollection: self organization protocolNames hasSameElements: #( 'empty' 'one' ).
    self assertCollection: (self organization protocolNamed: 'one') methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (self organization protocolNamed: 'empty') methodSelectors.

    "Now lets check that the new org has the same"
    newOrganization := ClassOrganization new
                           setSubject: self organization organizedClass;
                           copyFrom: self organization;
                           yourself.

    self assertCollection: newOrganization protocolNames hasSameElements: #( 'empty' 'one' ).
    self assertCollection: (newOrganization protocolNamed: 'one') methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (newOrganization protocolNamed: 'empty') methodSelectors.

    "And now lets check that updating one does not update the other."
    self organization addProtocol: 'two'.
    newOrganization classify: 'new' under: 'init'.

    self assertCollection: self organization protocolNames hasSameElements: #( 'empty' 'one' 'two' ).
    self assertCollection: (self organization protocolNamed: 'one') methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (self organization protocolNamed: 'empty') methodSelectors.
    self assertEmpty: (self organization protocolNamed: 'two') methodSelectors.

    self assertCollection: newOrganization protocolNames hasSameElements: #( 'empty' 'one' 'init' ).
    self assertCollection: (newOrganization protocolNamed: 'one') methodSelectors hasSameElements: #( 'one' ).
    self assertEmpty: (newOrganization protocolNamed: 'empty') methodSelectors.
    self assertCollection: (newOrganization protocolNamed: 'init') methodSelectors hasSameElements: #( 'new' )
]

{ #category : #tests }
ClassOrganizationTest >> testHasProtocol [

	self assert: (organization hasProtocol: #one).
	self deny: (organization hasProtocol: #two).

	self assert: (organization hasProtocol: (organization protocolNamed: #one)).
	self deny: (organization hasProtocol: (self class organization protocolNamed: #tests))
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
ClassOrganizationTest >> testRemoveElement [

	organization classify: #king under: #owl.
	organization classify: #luz under: #owl.
	self assert: (organization hasProtocol: #owl).
	self assertCollection: (organization protocolNamed: #owl) methodSelectors hasSameElements: #( #king #luz ).

	organization removeElement: #king.
	self assert: (organization hasProtocol: #owl).
	self assertCollection: (organization protocolNamed: #owl) methodSelectors hasSameElements: #( #luz ).

	organization removeElement: #luz.
	self deny: (organization hasProtocol: #owl)
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
	self assertCollection: (organization protocolNamed: #one) methodSelectors hasSameElements: #( one oneBis ).
	self assertCollection: (organization protocolNamed: #two) methodSelectors hasSameElements: #( two ).

	"Now that we asserted the actual state is good, we can test the actual method."
	class methodDict
		removeKey: #oneBis;
		removeKey: #two.
	organization removeNonexistentSelectorsFromProtocols.

	self assertCollection: organization protocolNames hasSameElements: #( one ).
	self assertCollection: (organization protocolNamed: #one) methodSelectors hasSameElements: #( one )
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveNonexistingProtocol [

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

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAs [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).
	self organization renameProtocol: #one as: #two.

	self assert: (self organization hasProtocol: #two).
	self deny: (self organization hasProtocol: #one).
	self assertCollection: (self organization protocolNamed: #two) methodSelectors hasSameElements: #( 'one' ).
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithExistingProtocol [

	self organization classify: 'king' under: 'two'.

	self assert: (self organization hasProtocol: #one).
	self assert: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).
	self assertCollection: (self organization protocolNamed: #two) methodSelectors hasSameElements: #( 'king' ).
	self organization renameProtocol: #one as: #two.

	self assert: (self organization hasProtocol: #two).
	self deny: (self organization hasProtocol: #one).
	self assertCollection: (self organization protocolNamed: #two) methodSelectors hasSameElements: #( 'one' 'king' )
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
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).

	self organization renameProtocol: #one as: nil.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil2 [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).

	self organization renameProtocol: nil as: #two.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNil3 [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).

	self organization renameProtocol: nil as: nil.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' )
]

{ #category : #tests }
ClassOrganizationTest >> testRenameProtocolAsWithNonExistingProtocol [

	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' ).

	self organization renameProtocol: #two as: #one.
	"Check that nothing changed."
	self assert: (self organization hasProtocol: #one).
	self deny: (self organization hasProtocol: #two).
	self assertCollection: (self organization protocolNamed: #one) methodSelectors hasSameElements: #( 'one' )
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
