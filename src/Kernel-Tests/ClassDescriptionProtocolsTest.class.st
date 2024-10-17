"
I am a test case responsible of testing ClassDescription protocol management.
"
Class {
	#name : 'ClassDescriptionProtocolsTest',
	#superclass : 'AbstractClassDescriptionTest',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testAddProtocol [

	class addProtocol: #titan.

	self assert: (class hasProtocol: #titan)
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testAddProtocolWithExistingProtocol [

	| titan |
	titan := class addProtocol: #titan.

	self assert: (class hasProtocol: titan).

	"Adding a second time should just return the same instance."
	self assert: (class addProtocol: #titan) identicalTo: titan
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testAddProtocolWithProtocolFromOtherClass [

	| titan |
	"This protocol is not from the class we are testing."
	titan := Protocol named: #titan.

	class addProtocol: titan.

	"The class should have created a new protocol of the same name."
	self assert: (class hasProtocol: #titan).
	self deny: (class protocolNamed: #titan) identicalTo: titan.
	self assert: (class protocolNamed: #titan) owningClass equals: class
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testAddProtocolWithRealProtocol [

	| titan |
	titan := class addProtocol: #titan.

	self assert: (class hasProtocol: titan).

	"Adding a second time should just return the same instance."
	self assert: (class addProtocol: titan) identicalTo: titan
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnder [

	"Lets create a new protocol via classification"
	class classify: #edalyn under: #which.
	class classify: #luz under: #which.

	self assertCollection: class protocolNames hasSameElements: #( #which ).
	self assertCollection: (class selectorsInProtocol: #which) hasSameElements: #( #edalyn #luz ).

	"Move a method"
	class classify: #luz under: #human.
	self assertCollection: class protocolNames hasSameElements: #( #human #which ).
	self assertCollection: (class selectorsInProtocol: #which) hasSameElements: #( #edalyn ).
	self assertCollection: (class selectorsInProtocol: #human) hasSameElements: #( #luz ).

	"Move last method"
	class classify: #edalyn under: #beast.
	self assertCollection: class protocolNames hasSameElements: #( #human #beast ).
	self assertCollection: (class selectorsInProtocol: #human) hasSameElements: #( #luz ).
	self assertCollection: (class selectorsInProtocol: #beast) hasSameElements: #( #edalyn ).

	"Nothing should change if the new protocol is the same than the old one"
	class classify: #edalyn under: #beast.
	self assertCollection: class protocolNames hasSameElements: #( #human #beast ).
	self assertCollection: (class selectorsInProtocol: #human) hasSameElements: #( #luz ).
	self assertCollection: (class selectorsInProtocol: #beast) hasSameElements: #( #edalyn )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnderExtensionFromTheSamePackage [
	"If we classify a method as an extension of it's own package, we expect it to end up unclassified."

	self
		should: [
			class compiler
				protocol: ('*' , class package name) asSymbol;
				install: 'king ^ 1' ]
		raise: ExtensionPointsOwningPackageNotification.
	[
	class compiler
		protocol: ('*' , class package name) asSymbol;
		install: 'king ^ 1' ]
		on: ExtensionPointsOwningPackageNotification
		do: [ :e | e resume ].

	self deny: (class >> #king) isClassified.
	self deny: (class >> #king) protocol isExtensionProtocol
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnderUnclassified [
	"Ensure unclassified is acting as any other protocol because that was not the case in the past."

	"Lets create a new protocol via classification"
	class classify: #king under: Protocol unclassified.
	class classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).
	
	"This should do nothing."
	class classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz ).

	"Now we move a method from unclassified to another protocol."
	class classify: #luz under: #human.

	self assertCollection: class protocolNames hasSameElements: { #human. Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king ).
	
	"Now we move back to unclassified."
	class classify: #luz under: Protocol unclassified.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king #luz )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnderWithNil [
	"Set the base for the test"
	| unclassified|
	unclassified := Protocol unclassified.

	class classify: #king under: nil.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king ).
	
	class classify: #luz under: #human.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified . #human }.
	self assertCollection: (class selectorsInProtocol: #human) hasSameElements: #( #luz ).
	
	"In case we classify under nil while the method already has a protocol, we do not want to update the protocol."
	class classify: #luz under: nil.

	self assertCollection: class protocolNames hasSameElements: { Protocol unclassified . #human }.
	self assertCollection: class uncategorizedSelectors hasSameElements: #( #king ).
	self assertCollection: (class selectorsInProtocol: #human) hasSameElements: #( #luz )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnderWithProtocol [

	"Lets create a new protocol via classification"
	class classify: #edalyn under: #which.
	class classify: #luz under: (class protocolNamed: #which).

	self assertCollection: class protocolNames hasSameElements: #( #which ).
	self assertCollection: (class selectorsInProtocol: #which) hasSameElements: #( #edalyn #luz )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testClassifyUnderWithProtocolFromOtherClass [

	| which |
	class classify: #luz under: (which := Protocol named: #which).

	self assertCollection: class protocolNames hasSameElements: #( #which ).
	self assertCollection: (class selectorsInProtocol: #which) hasSameElements: #( #luz ).

	"Since the protocol was not from the class, it should not be the same instance."
	self deny: (class protocolNamed: #which) identicalTo: which.
	self assert: (class protocolNamed: #which) owningClass equals: class
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testEnsureProtocol [

	| protocol outsideProtocol |
	class addProtocol: #titan.
	protocol := class protocolNamed: #titan.
	outsideProtocol := Protocol named: #titan.

	self assertCollection: class protocolNames hasSameElements: { #titan }.
	self assert: (class ensureProtocol: #titan) equals: protocol.
	self assert: (class ensureProtocol: #titan) owningClass equals: class.
	self assert: (class ensureProtocol: protocol) equals: protocol.
	self assert: (class ensureProtocol: outsideProtocol) equals: protocol.
	self deny: (class ensureProtocol: outsideProtocol) identicalTo: outsideProtocol.

	self assert: (class ensureProtocol: nil) name equals: Protocol unclassified.
	self assertCollection: class protocolNames hasSameElements: {
			#titan.
			Protocol unclassified }.

	self assert: (class ensureProtocol: #human) equals: (class protocolNamed: #human).
	self assertCollection: class protocolNames hasSameElements: {
			#titan.
			#human.
			Protocol unclassified }
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testExtensionProtocols [

	| extensionPackageName |
	extensionPackageName := class package name , '2'.
	class addProtocol: #titan.
	[
	class addProtocol: '*' , extensionPackageName.

	self assertCollection: class extensionProtocols hasSameElements: { (class protocolNamed: '*' , extensionPackageName) } ] ensure: [
		self packageOrganizer removePackage: extensionPackageName ]
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testHasProtocol [

	class addProtocol: #titan.
	self assert: (class hasProtocol: #titan).
	self deny: (class hasProtocol: #human).

	self assert: (class hasProtocol: (class protocolNamed: #titan)).
	self deny: (class hasProtocol: (Protocol named: #human))
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testHasProtocolWithProtocolFromOtherClass [

	class addProtocol: #titan.
	self assert: (class hasProtocol: (Protocol named: #titan))
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testHasProtocolWithRealProtocol [

	class addProtocol: #titan.
	self assert: (class hasProtocol: (class protocolNamed: #titan)).
	self deny: (class hasProtocol: (Protocol named: #human))
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocolNameOfSelector [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: (class protocolNameOfSelector: #king) equals: #titan.
	"In the future this should maybe be an error?"
	self assert: (class protocolNameOfSelector: #luz) isNil
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocolNamed [

	| titanProtocol |
	titanProtocol := class addProtocol: #titan.
	self assert: (class protocolNamed: #titan) identicalTo: titanProtocol.
	self should: [ class protocolNamed: #human ] raise: NotFound
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocolNamedIfAbsent [

	| titanProtocol |
	titanProtocol := class addProtocol: #titan.
	self assert: (class protocolNamed: #titan ifAbsent: [ self fail ]) identicalTo: titanProtocol.
	self assert: [ class protocolNamed: #human ifAbsent: [ true ] ]
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocolNames [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.

	self assertCollection: class protocolNames hasSameElements: #( #titan #human #witch ).

	class removeProtocolIfEmpty: #titan.

	self assertCollection: class protocolNames hasSameElements: #( #human #witch )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocolOfSelector [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: (class protocolOfSelector: #king) identicalTo: (class protocolNamed: #titan).
	self assert: (class protocolOfSelector: #king) name equals: #titan.
	"In the future this should maybe be an error?"
	self assert: (class protocolOfSelector: #luz) isNil
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testProtocols [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.

	self assertCollection: class protocols hasSameElements: (#( #titan #human #witch ) collect: [ :protocolName | class protocolNamed: protocolName ]).

	class removeProtocolIfEmpty: #titan.

	self assertCollection: class protocols hasSameElements: (#( #human #witch ) collect: [ :protocolName | class protocolNamed: protocolName ])
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveFromProtocols [

	class classify: #amity under: #witch.
	class classify: #edalyn under: #witch.
	self assert: (class hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #amity #edalyn ).

	class removeFromProtocols: #amity.
	self assert: (class hasProtocol: #witch).
	self assertCollection: (class selectorsInProtocol: #witch) hasSameElements: #( #edalyn ).

	class removeFromProtocols: #edalyn.
	self deny: (class hasProtocol: #witch)
]

{ #category : 'tests' }
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

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocol [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.
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

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmpty [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.
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

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmptyOnNonExistingProtocol [

	self deny: (class hasProtocol: #titan).
	self shouldnt: [ class removeProtocolIfEmpty: #titan ] raise: Error
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmptyWithProtocolFromOtherClass [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.
	class compiler
		protocol: #titan;
		install: 'king ^1'.
	self assert: class protocolNames size equals: 3.
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king ).

	"just ignore removing of non empty categories"
	class removeProtocolIfEmpty: (Protocol named: #titan).
	self assert: class protocolNames size equals: 3
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolIfEmptyWithRealProtocol [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.
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

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolOnNonExistingProtocol [

	self deny: (class hasProtocol: #titan).
	self shouldnt: [ class removeProtocol: #titan ] raise: Error
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRemoveProtocolWithRealProtocol [

	class addProtocol: #titan.
	class addProtocol: #human.
	class addProtocol: #witch.
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

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAs [

	class compiler
		protocol: #demon;
		install: 'king ^1'.

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).
	class renameProtocol: #demon as: #titan.

	self assert: (class hasProtocol: #titan).
	self deny: (class hasProtocol: #demon).
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsAnExtensionOfThePackageContainingTheClass [
	"We should not be able to have a protocol that is an extension of the package containing the class"

	| extension |
	class compiler
		protocol: #demon;
		install: 'king ^1'.

	extension := '*' , class package name.

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: extension).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).

	self should: [ class renameProtocol: #demon as: extension ] raise: ExtensionPointsOwningPackageNotification.
	[ class renameProtocol: #demon as: extension ]
		on: ExtensionPointsOwningPackageNotification
		do: [ :e | e resume ].

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: extension).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithExistingProtocol [

	class
		addProtocol: #titan;
		classify: #king under: #demon.

	self assert: (class hasProtocol: #titan).
	self assert: (class hasProtocol: #demon).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).
	self assertEmpty: (class selectorsInProtocol: #titan).
	class renameProtocol: #demon as: #titan.

	self assert: (class hasProtocol: #titan).
	self deny: (class hasProtocol: #demon).
	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithExistingProtocolWithProtocol [

	| titan demon |
	class
		addProtocol: #titan;
		classify: #king under: #demon.

	titan := class protocolNamed: #titan.
	demon := class protocolNamed: #demon.

	self assert: (class hasProtocol: titan).
	self assert: (class hasProtocol: demon).
	self assertEmpty: titan methodSelectors.
	self assertCollection: demon methodSelectors hasSameElements: #( #king ).
	class renameProtocol: titan as: demon.

	self assert: (class hasProtocol: demon).
	self deny: (class hasProtocol: titan).
	self assertCollection: demon methodSelectors hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithNil [

	class classify: #king under: #demon.

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).

	class renameProtocol: #demon as: nil.
	"Check that nothing changed."
	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithNil2 [

	class classify: #king under: #demon.

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).

	class renameProtocol: nil as: #titan.
	"Check that nothing changed."
	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithNil3 [

	class classify: #king under: #demon.

	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).

	class renameProtocol: nil as: nil.
	"Check that nothing changed."
	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithNonExistingProtocol [

	class classify: #king under: #demon.
	
	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king ).

	class renameProtocol: #two as: #demon.
	"Check that nothing changed."
	self assert: (class hasProtocol: #demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: (class selectorsInProtocol: #demon) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testRenameProtocolAsWithProtocol [

	| demon titan |
	class classify: #king under: #demon.

	demon := class protocolNamed: #demon.
	self assert: (class hasProtocol: demon).
	self deny: (class hasProtocol: #titan).
	self assertCollection: demon methodSelectors hasSameElements: #( #king ).
	class renameProtocol: demon as: #titan.

	titan := class protocolNamed: #titan.
	self assert: (class hasProtocol: titan).
	self deny: (class hasProtocol: demon).
	self assertCollection: titan methodSelectors hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testSelectorsInProtocol [

	self assertEmpty: (class selectorsInProtocol: #titan).

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assertCollection: (class selectorsInProtocol: #titan) hasSameElements: #( #king )
]

{ #category : 'tests' }
ClassDescriptionProtocolsTest >> testSelectorsInProtocolWithRealProtocol [

	self assertEmpty: (Protocol named: #titan).

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assertCollection: (class selectorsInProtocol: (class protocolNamed: #titan)) hasSameElements: #( #king )
]
