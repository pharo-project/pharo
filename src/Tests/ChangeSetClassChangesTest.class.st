"
Class category changes are not being properly added to the default changeset in Squeak 3.7. This test case will pass in Squeak 3.6, and fail in Squeak 3.[7-9].

"
Class {
	#name : #ChangeSetClassChangesTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'factory',
		'oldChangeSet',
		'newChangeSetName'
	],
	#category : #'Tests-System'
}

{ #category : #support }
ChangeSetClassChangesTest >> assertDefinition: actualString equivalentTo: expectedString [ 
	"When a class definition is reconstructed with #fatDefForClass, it may 
	contain extra trailing space characters in parts of the definition. This 
	is probably a minor bug, but it should be overlooked for purposes of 
	testing the change set update mechanism. The expedient here is to just 
	remove spaces before comparing the definition strings."

	^ self
		assert: actualString 
		equals: expectedString
]

{ #category : #support }
ChangeSetClassChangesTest >> denyDefinition: actualString equivalentTo: expectedString [ 
	"When a class definition is reconstructed with #fatDefForClass, it may 
	contain extra trailing space characters in parts of the definition. This 
	is probably a minor bug, but it should be overlooked for purposes of 
	testing the change set update mechanism. The expedient here is to just 
	remove spaces before comparing the definition strings."

	^ self
		deny: (actualString copyReplaceAll: ' ''' with: '''')
		equals: (expectedString copyReplaceAll: ' ''' with: '''')
]

{ #category : #running }
ChangeSetClassChangesTest >> setUp [

	super setUp.
	oldChangeSet := ChangeSet current.
	ChangeSet newChanges: (ChangeSet newTestChangeSet).
	newChangeSetName := ChangeSet current name.
	factory := ClassFactoryForTestCase new.
]

{ #category : #running }
ChangeSetClassChangesTest >> tearDown [
	factory cleanUp.
	ChangeSet newChanges: oldChangeSet.
	ChangeSet removeChangeSetsNamedSuchThat: [ :each | each = newChangeSetName ].
	super tearDown
]

{ #category : #tests }
ChangeSetClassChangesTest >> testAddInstanceVariable [

	"Adding an instance variable to the class should result in a change
	record being added to the current change set."

	| class className saveClassDefinition |
	"Define a class and save its definition"
	class := factory newClassWithInstanceVariableNames: 'zzz'.
	className := class name.
	saveClassDefinition := class definitionString.

	self
		assertDefinition: saveClassDefinition
		equivalentTo: class definitionString.
	
	"Redefine the class, adding one instance variable"
	factory redefineClass: class instanceVariableNames: 'zzz aaa'.

	self
		denyDefinition: class definitionString
		equivalentTo: saveClassDefinition.
	
	"Assert that the change has been recorded in the current change set"
	self
		assertDefinition: (ChangeSet current changeRecorderFor: class) priorDefinition
		equivalentTo: saveClassDefinition
]

{ #category : #tests }
ChangeSetClassChangesTest >> testAddInstanceVariableAddsNewChangeRecord [
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	| class saveClassDefinition |

	class := factory newClassWithInstanceVariableNames: 'zzz'.
 	saveClassDefinition := class definitionString.
	ChangeSet current removeClassChanges: class name.	
	"Redefine the class, adding one instance variable"
	factory redefineClass: class instanceVariableNames: 'zzz aaa'.

	self
		assertDefinition: (ChangeSet current changeRecorderFor: class name) priorDefinition
		equivalentTo: saveClassDefinition
]

{ #category : #tests }
ChangeSetClassChangesTest >> testChangeClassCategory [
	"Changing the class category for a class should result in a change
	record being added to the current change set."

	| class saveClassDefinition |
	"Define a class and save its definition"
	
	class := factory newClassInCategory: 'TestPackage1'.
	saveClassDefinition := class definitionString.
	self 
		assert: saveClassDefinition 
		equals: class definitionString.
	
	"Redefine the class, changing only the class category"
	factory redefineClass: class category: 'TestPackage2'. 
	self 
		denyDefinition: class definitionString
		equivalentTo: saveClassDefinition.
			
	"Assert that the change has been recorded in the current change set"
	self
		assertDefinition: (ChangeSet current changeRecorderFor: class) priorDefinition
		equivalentTo: class definitionString
]

{ #category : #tests }
ChangeSetClassChangesTest >> testChangeClassCategoryAddsNewChangeRecord [
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	"At the start of this test, JunkClass should not exist, and there should be
	no change records pertaining to it in the change set."

	| class |
	class := factory newClassInCategory: 'TestPackage1'.
	
	"Forget about JunkClass in the change set"
	ChangeSet current removeClassChanges: class name. 
	
	"Redefine the class, changing only the class category"
	factory redefineClass: class category: 'TestPackage2'.
	
	self
		assertDefinition: (ChangeSet current changeRecorderFor: class) priorDefinition
		equivalentTo: class definitionString
]

{ #category : #tests }
ChangeSetClassChangesTest >> testInitialChangeSet [
	"Run this to assure the initial changeset is named. Checks bug found in 3.9 7052."

	self deny: ChangeSet current printString equals: 'a ChangeSet named <no name -- garbage?>'
]

{ #category : #support }
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
