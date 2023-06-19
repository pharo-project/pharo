Class {
	#name : #MCWorkingCopyManagementTest,
	#superclass : #MCTestCase,
	#instVars : [
		'package2',
		'package1',
		'mcPackage1',
		'mcPackage2',
		'classToRemove'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #running }
MCWorkingCopyManagementTest >> setUp [

	super setUp.
	package1 := self packageOrganizer createPackageNamed: #A , UUID new asString36.
	package2 := self packageOrganizer createPackageNamed: package1 name , #'-SubPart'.
	mcPackage1 := package1 mcPackage.
	mcPackage2 := package2 mcPackage
]

{ #category : #running }
MCWorkingCopyManagementTest >> tearDown [

	classToRemove ifNotNil: [ classToRemove removeFromSystem ].

	package1 unregister.
	package2 unregister.
	MCWorkingCopy registry
		removeKey: mcPackage1 ifAbsent: [  ];
		removeKey: mcPackage2 ifAbsent: [  ].
	super tearDown
]

{ #category : #tests }
MCWorkingCopyManagementTest >> testClassAdded [

	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified not.
	
	classToRemove := self class classInstaller make: [ :aBuilder | 
			aBuilder 
				name: ('A' , UUID new asString36) asSymbol;
				package: package2 name].
	
	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified
	

]

{ #category : #tests }
MCWorkingCopyManagementTest >> testClassRemoved [
	
	classToRemove := self class classInstaller make: [ :aBuilder | 
			aBuilder 
				name: ('A' , UUID new asString36) asSymbol;
				package: package2 name].
	
	package1 mcWorkingCopy modified: false.
	package2 mcWorkingCopy modified: false.
	
	classToRemove removeFromSystem.
	classToRemove := nil.
	
	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified.
	
	
	
]

{ #category : #tests }
MCWorkingCopyManagementTest >> testManagersForCategoryDo [
	"Consider the following package structure:
		Renraku
		Renraku-Help

		and a method extension with the protocol '*renraku'.
		MCWorkingCopy>>methodModified: should only mark 'Renraku' as modified,
		not 'Renraku-Help'"

	| managers |
	self assert: (MCWorkingCopy registry includesKey: mcPackage1).
	self assert: (MCWorkingCopy registry includesKey: mcPackage2).

	managers := OrderedCollection new.
	MCWorkingCopy managersForCategory: mcPackage1 name do: [ :manager | managers add: manager ].

	self assert: managers size equals: 1.
	self assert: managers first package identicalTo: mcPackage1.

	managers := OrderedCollection new.
	MCWorkingCopy managersForCategory: mcPackage2 name do: [ :manager | managers add: manager ].

	self assert: managers size equals: 1.
	self assert: managers first package identicalTo: mcPackage2
]
