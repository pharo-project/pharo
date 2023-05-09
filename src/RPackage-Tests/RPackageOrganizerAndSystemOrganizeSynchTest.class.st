"
Currently the system maintains 2 packages organisers. 
- RPackageOrganizer default
- SystemOrganizer default

Both of them should stay in synch and this test class will ensure that. 

Let's note that SystemOrganizer should vanish in the future so I'll probably vanish too, but in the meantime I'll ensure that we are in sync.
"
Class {
	#name : #RPackageOrganizerAndSystemOrganizeSynchTest,
	#superclass : #TestCase,
	#category : #'RPackage-Tests'
}

{ #category : #helpers }
RPackageOrganizerAndSystemOrganizeSynchTest >> packageName [
	^ 'RPackageOragizationSynchPackageTest'
]

{ #category : #running }
RPackageOrganizerAndSystemOrganizeSynchTest >> tearDown [

	(self packageName asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ].
	Smalltalk organization systemOrganizer  removeCategory: self packageName.
	Smalltalk organization systemOrganizer  removeCategoriesMatching: self packageName , '-*'.
	super tearDown
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testAddingPackage [

	Smalltalk organization systemOrganizer  addCategory: self packageName.

	self assert: (Smalltalk organization packageNames includes: self packageName).
	self assert: (Smalltalk organization systemOrganizer  includesCategory: self packageName)
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testRemovingPackage [
	"Regression test because removing a RPackage from the system was not removing the category in SystemOrganizer."

	Smalltalk organization systemOrganizer  addCategory: self packageName.

	self assert: (Smalltalk organization packageNames includes: self packageName).
	self assert: (Smalltalk organization systemOrganizer  includesCategory: self packageName).

	self packageName asPackage removeFromSystem.

	self deny: (Smalltalk organization packageNames includes: self packageName).
	self deny: (Smalltalk organization systemOrganizer  includesCategory: self packageName)
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testRemovingPackageWithTags [
	"Regression test because removing a RPackage from the system was not removing the category in SystemOrganizer."

	Smalltalk organization systemOrganizer addCategory: self packageName.
	(Object << 'RPackageOragizationSynchClass1')
		package: self packageName;
		tag: 'tag1';
		install.
	(Object << 'RPackageOragizationSynchClass2')
		package: self packageName;
		tag: 'tag2';
		install.
	self assert: (Smalltalk organization packageNames includes: self packageName).
	self assert: (Smalltalk organization systemOrganizer  includesCategory: self packageName).
	self assert: (Smalltalk organization systemOrganizer  includesCategory: self packageName , '-tag1').
	self assert: (Smalltalk organization systemOrganizer  includesCategory: self packageName , '-tag2').

	self packageName asPackage removeFromSystem.

	self deny: (Smalltalk organization packageNames includes: self packageName).
	self deny: (Smalltalk organization systemOrganizer  includesCategory: self packageName).
	self deny: (Smalltalk organization systemOrganizer  includesCategory: self packageName , '-tag1').
	self deny: (Smalltalk organization systemOrganizer  includesCategory: self packageName , '-tag2')
]
