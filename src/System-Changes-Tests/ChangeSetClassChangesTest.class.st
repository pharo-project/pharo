"
Class category changes are not being properly added to the default changeset in Squeak 3.7. This test case will pass in Squeak 3.6, and fail in Squeak 3.[7-9].

"
Class {
	#name : 'ChangeSetClassChangesTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'factory',
		'oldChangeSet',
		'newChangeSetName'
	],
	#category : 'System-Changes-Tests-Base',
	#package : 'System-Changes-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ChangeSetClassChangesTest >> setUp [

	super setUp.
	oldChangeSet := ChangeSet current.
	ChangeSet newChanges: (ChangeSet newTestChangeSet).
	newChangeSetName := ChangeSet current name.
	factory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ChangeSetClassChangesTest >> tearDown [
	factory cleanUp.
	ChangeSet newChanges: oldChangeSet.
	ChangeSet removeChangeSetsNamedSuchThat: [ :each | each = newChangeSetName ].
	super tearDown
]

{ #category : 'tests' }
ChangeSetClassChangesTest >> testAddInstanceVariable [
	"Adding an instance variable to the class should result in a change
	record being added to the current change set."

	| class className saveClassDefinition |
	"Define a class and save its definition"
	class := factory make: [ :aBuilder | aBuilder slots: #(zzz) ].
	className := class name.
	saveClassDefinition := class definitionString.

	self assert: saveClassDefinition equals: class definitionString.

	"Redefine the class, adding one instance variable"
	factory make: [ :aBuilder |
		aBuilder
			fillFor: class;
			slots: #(zzz aaa) ].

	self deny: class definitionString equals: saveClassDefinition.

	"Assert that the change has been recorded in the current change set"
	self assert: (ChangeSet current changeRecorderFor: class) priorDefinition equals: saveClassDefinition
]

{ #category : 'tests' }
ChangeSetClassChangesTest >> testAddInstanceVariableAddsNewChangeRecord [
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	| class saveClassDefinition |
	class := factory make: [ :aBuilder | aBuilder slots: #(zzz) ].
	saveClassDefinition := class definitionString.
	ChangeSet current removeClassChanges: class name.
	"Redefine the class, adding one instance variable"
	factory make: [ :aBuilder |
		aBuilder
			fillFor: class;
			slots: #(zzz aaa) ].

	self assert: (ChangeSet current changeRecorderFor: class name) priorDefinition equals: saveClassDefinition
]

{ #category : 'tests' }
ChangeSetClassChangesTest >> testChangeClassCategory [
	"Changing the class category for a class should result in a change
	record being added to the current change set."

	[
	| class saveClassDefinition |
	"Define a class and save its definition"
	class := factory make: [ :aBuilder | aBuilder package: 'TestPackage1' ].
	saveClassDefinition := class definitionString.
	self assert: saveClassDefinition equals: class definitionString.

	"Redefine the class, changing only the class category"
	factory make: [ :aBuilder |
		aBuilder
			fillFor: class;
			package: 'TestPackage2' ].
	self deny: class definitionString equals: saveClassDefinition.

	"Assert that the change has been recorded in the current change set"
	self assert: (ChangeSet current changeRecorderFor: class) priorDefinition equals: class definitionString ] ensure: [
		self packageOrganizer
			removePackage: 'TestPackage1';
			removePackage: 'TestPackage2' ]
]

{ #category : 'tests' }
ChangeSetClassChangesTest >> testChangeClassCategoryAddsNewChangeRecord [
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	"At the start of this test, JunkClass should not exist, and there should be
	no change records pertaining to it in the change set."

	[
	| class |
	class := factory make: [ :aBuilder | aBuilder package: 'TestPackage1' ].

	"Forget about JunkClass in the change set"
	ChangeSet current removeClassChanges: class name.

	"Redefine the class, changing only the class category"
	factory make: [ :aBuilder |
		aBuilder
			fillFor: class;
			package: 'TestPackage2' ].

	self assert: (ChangeSet current changeRecorderFor: class) priorDefinition equals: class definitionString ] ensure: [
		self packageOrganizer
			removePackage: 'TestPackage1';
			removePackage: 'TestPackage2' ]
]

{ #category : 'tests' }
ChangeSetClassChangesTest >> testInitialChangeSet [
	"Run this to assure the initial changeset is named. Checks bug found in 3.9 7052."

	self deny: ChangeSet current printString equals: 'a ChangeSet named <no name -- garbage?>'
]

{ #category : 'support' }
ChangeSetClassChangesTest >> testRenaming [

	| class newMetaclassName renamedName |
	class := factory newClass.
	renamedName := ('Renamed' , class name) asSymbol.
	newMetaclassName := (renamedName , #' class') asSymbol.
	class class compile: 'dummyMeth'.
	class rename: renamedName.
	self assert: class name equals: renamedName.
	self assert: (ChangeSet current changedClassNames includes: renamedName).
	self assert: (ChangeSet current changedClassNames includes: newMetaclassName)
]
