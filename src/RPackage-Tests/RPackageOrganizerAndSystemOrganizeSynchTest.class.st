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
	self packageOrganizer removeCategory: self packageName.
	self packageOrganizer removeCategoriesMatching: self packageName , '-*'.
	super tearDown
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testAddingPackage [

	self packageOrganizer addCategory: self packageName.

	self assert: (self packageOrganizer packageNames includes: self packageName).
	self assert: (self packageOrganizer includesCategory: self packageName)
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testRemovingPackage [
	"Regression test because removing a RPackage from the system was not removing the category in SystemOrganizer."

	self packageOrganizer addCategory: self packageName.

	self assert: (self packageOrganizer packageNames includes: self packageName).
	self assert: (self packageOrganizer includesCategory: self packageName).

	self packageName asPackage removeFromSystem.

	self deny: (self packageOrganizer packageNames includes: self packageName).
	self deny: (self packageOrganizer includesCategory: self packageName)
]

{ #category : #tests }
RPackageOrganizerAndSystemOrganizeSynchTest >> testRemovingPackageWithTags [
	"Regression test because removing a RPackage from the system was not removing the category in SystemOrganizer."

	self packageOrganizer addCategory: self packageName.
	(Object << 'RPackageOragizationSynchClass1')
		package: self packageName;
		tag: 'tag1';
		install.
	(Object << 'RPackageOragizationSynchClass2')
		package: self packageName;
		tag: 'tag2';
		install.
	self assert: (self packageOrganizer packageNames includes: self packageName).
	self assert: (self packageOrganizer includesCategory: self packageName).
	self assert: (self packageOrganizer includesCategory: self packageName , '-tag1').
	self assert: (self packageOrganizer includesCategory: self packageName , '-tag2').

	self packageName asPackage removeFromSystem.

	self deny: (self packageOrganizer packageNames includes: self packageName).
	self deny: (self packageOrganizer includesCategory: self packageName).
	self deny: (self packageOrganizer includesCategory: self packageName , '-tag1').
	self deny: (self packageOrganizer includesCategory: self packageName , '-tag2')
]
