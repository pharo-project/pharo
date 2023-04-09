Class {
	#name : #ClassRenameFixTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'previousChangeSet',
		'testsChangeSet',
		'newClassName',
		'originalName'
	],
	#category : #'System-Changes-Tests-Base'
}

{ #category : #tests }
ClassRenameFixTest >> packageName [

	^ 'ClassRenameFix-GeneradClass'
]

{ #category : #running }
ClassRenameFixTest >> setUp [

	super setUp.
	SystemAnnouncer uniqueInstance weak when: ClassRenamed send: #verifyRenameEvent: to: self
]

{ #category : #running }
ClassRenameFixTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	(self packageName asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ].
	super tearDown
]

{ #category : #tests }
ClassRenameFixTest >> testRenameClassUsingClass [

	| createdClass foundClasses |
	originalName := #ClassRenameMockClass1.

	createdClass := self class classInstaller make: [ :aBuilder |
		                aBuilder
			                name: originalName;
			                package: self packageName ].

	newClassName := #ClassRenameMockClass2.
	createdClass rename: newClassName.
	self assert: (testingEnvironment classNamed: originalName) isNil.
	self assert: (testingEnvironment classNamed: newClassName) isNotNil.
	foundClasses := testingEnvironment organization listAtCategoryNamed: self packageName.

	self assert: (foundClasses includes: newClassName).
	self assert: createdClass name equals: newClassName
]

{ #category : #running }
ClassRenameFixTest >> verifyRenameEvent: aRenamedEvent [
	| renamedClass |
	renamedClass := aRenamedEvent classRenamed.
	self assert: (testingEnvironment classNamed: newClassName) name equals: newClassName.
	self assert: renamedClass name equals: newClassName
]
