"
Class category changes are not being properly added to the default changeset in Squeak 3.7. This test case will pass in Squeak 3.6, and fail in Squeak 3.[7-9].

"
Class {
	#name : #ChangeSetClassChangesTest,
	#superclass : #TestCase,
	#instVars : [
		'className',
		'renamedName'
	],
	#category : #'Tests-System'
}

{ #category : #running }
ChangeSetClassChangesTest >> categoryNameForTemporaryClasses [
	"Answer the category where to classify temporarily created classes"

	^'Dummy-Tests-Class' 
] 

{ #category : #support }
ChangeSetClassChangesTest >> deleteClass [
	| cl |
	cl := Smalltalk globals at: className ifAbsent: [ ^ self ].
	cl
		removeFromChanges;
		removeFromSystemUnlogged
]

{ #category : #support }
ChangeSetClassChangesTest >> deleteRenamedClass [
	| cl |
	cl := Smalltalk globals at: renamedName ifAbsent: [ ^ self ].
	cl
		removeFromChanges;
		removeFromSystemUnlogged
]

{ #category : #support }
ChangeSetClassChangesTest >> isDefinition: firstString equivalentTo: secondString [ 
	"When a class definition is reconstructed with #fatDefForClass, it may 
	contain extra trailing space characters in parts of the definition. This 
	is probably a minor bug, but it should be overlooked for purposes of 
	testing the change set update mechanism. The expedient here is to just 
	remove spaces before comparing the definition strings."

	^ firstString notNil
		and: [(firstString copyReplaceAll: ' ''' with: '''')
				= (secondString copyReplaceAll: ' ''' with: '''')]
]

{ #category : #running }
ChangeSetClassChangesTest >> setUp [
	super setUp.
	className := #TUTU.
	renamedName := #RenamedTUTU.
	self deleteClass.
	self deleteRenamedClass.
	Object subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses
]

{ #category : #running }
ChangeSetClassChangesTest >> tearDown [
	(Smalltalk globals classNamed: #JunkClass) ifNotNil: [ :c | c removeFromSystem: true ].
	SystemOrganization removeCategory: #'DeleteMe-1'.
	SystemOrganization removeCategory: #'DeleteMe-2'.
	ChangeSet current removeClassChanges: 'JunkClass'.
	super tearDown
]

{ #category : #testing }
ChangeSetClassChangesTest >> testAddInstanceVariable [
	"Adding an instance variable to the class should result in a change
	record being added to the current change set."

	| saveClassDefinition |
	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.
	saveClassDefinition := (Smalltalk globals classNamed: #JunkClass) definition.
	self
		assert:
			(self
				isDefinition: saveClassDefinition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).	"Redefine the class, adding one instance variable"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz aaa'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"Assert that the class definition has changed"
	self deny: (self isDefinition: (Smalltalk globals classNamed: #JunkClass) definition equivalentTo: saveClassDefinition).
	self
		deny:
			(self
				isDefinition: saveClassDefinition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).
	self
		assert:
			(self
				isDefinition: (Smalltalk globals classNamed: #JunkClass) definition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).	"Assert that the change has been recorded in the current change set"
	self
		assert:
			(self
				isDefinition: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) priorDefinition
				equivalentTo: saveClassDefinition)
]

{ #category : #testing }
ChangeSetClassChangesTest >> testAddInstanceVariableAddsNewChangeRecord [ 
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	"At the start of this test, JunkClass should not exist, and there should be
	no change records pertaining to it in the change set."

	self deny: (Smalltalk hasClassNamed: 'JunkClass').
	self assert: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) thisName = 'nil'.	"Remove bogus change records created as side effect of preceding assert"
	ChangeSet current removeClassChanges: 'nil'.	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"Forget about JunkClass in the change set"
	ChangeSet current removeClassChanges: 'JunkClass'.	"Redefine the class, adding one instance variable"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz aaa'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"A change record should now exist in the change set"
	self
		assert:
			(self
				isDefinition: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) priorDefinition
				equivalentTo:
					'Object subclass: #JunkClass
	instanceVariableNames: ''zzz ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''DeleteMe-1''')
]

{ #category : #testing }
ChangeSetClassChangesTest >> testChangeClassCategory [
	"Changing the class category for a class should result in a change
	record being added to the current change set."

	| saveClassDefinition |
	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.
	saveClassDefinition := (Smalltalk globals classNamed: #JunkClass) definition.
	self assert: saveClassDefinition = (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass)).	"Redefine the class, changing only the class category"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-2'.	"Assert that the class definition has changed"
	self deny: (self isDefinition: (Smalltalk globals classNamed: #JunkClass) definition equivalentTo: saveClassDefinition).
	self
		deny:
			(self
				isDefinition: saveClassDefinition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).
	self
		assert:
			(self
				isDefinition: (Smalltalk globals classNamed: #JunkClass) definition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).	"Assert that the change has been recorded in the current change set"
	self
		assert:
			(self
				isDefinition: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) priorDefinition
				equivalentTo:
					'Object subclass: #JunkClass
	instanceVariableNames: ''zzz ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''DeleteMe-2''')
]

{ #category : #testing }
ChangeSetClassChangesTest >> testChangeClassCategoryAddsNewChangeRecord [
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	"At the start of this test, JunkClass should not exist, and there should be
	no change records pertaining to it in the change set."

	self deny: (Smalltalk hasClassNamed: 'JunkClass').
	self assert: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) thisName = 'nil'.	"Remove bogus change records created as side effect of preceding assert"
	ChangeSet current removeClassChanges: 'nil'.	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"Forget about JunkClass in the change set"
	ChangeSet current removeClassChanges: 'JunkClass'.	"Redefine the class, changing only the class category"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-2'.	"A change record should now exist in the change set"
	self
		assert:
			(self
				isDefinition: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) priorDefinition
				equivalentTo:
					'Object subclass: #JunkClass
	instanceVariableNames: ''zzz ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''DeleteMe-2''')
]

{ #category : #testing }
ChangeSetClassChangesTest >> testInitialChangeSet [
	"Run this to assure the initial changeset is named. Checks bug found in 3.9 7052."

	self deny: (ChangeSet current printString = 'a ChangeSet named <no name -- garbage?>')
]

{ #category : #support }
ChangeSetClassChangesTest >> testRenaming [

	| oldName newMetaclassName class |
	oldName := className.
	newMetaclassName := (renamedName , #' class') asSymbol.
	class := Smalltalk globals at: oldName.
	class class compile: 'dummyMeth'.
	class rename: renamedName.
	self assert: class name equals: renamedName.
	self assert: (ChangeSet current changedClassNames includes: renamedName).
	self assert: (ChangeSet current changedClassNames includes: newMetaclassName)
]
