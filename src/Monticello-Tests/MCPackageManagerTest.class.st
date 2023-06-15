Class {
	#name : #MCPackageManagerTest,
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
MCPackageManagerTest >> setUp [

	super setUp.
	package1 := self packageOrganizer createPackageNamed: #A , UUID new asString36.
	package2 := self packageOrganizer createPackageNamed: package1 name , #'-SubPart'.
	mcPackage1 := package1 mcPackage.
	mcPackage2 := package2 mcPackage.

	"We automatically register working copies in MCWorkingCopy, but since we test MCPackageManager here, we need to explicitly register it here"
	MCPackageManager
		ensureForPackage: mcPackage1;
		ensureForPackage: mcPackage2
]

{ #category : #running }
MCPackageManagerTest >> tearDown [
	
	classToRemove ifNotNil: [ classToRemove removeFromSystem ].
	
	package1 unregister.
	package2 unregister.
	MCPackageManager registry
		removeKey: mcPackage1 ifAbsent: [ ];
		removeKey: mcPackage2 ifAbsent: [ ].
	super tearDown	
]

{ #category : #tests }
MCPackageManagerTest >> testClassAdded [

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
MCPackageManagerTest >> testClassRemoved [
	
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
MCPackageManagerTest >> testManagersForCategoryDo [
	"Consider the following package structure:
		Renraku
		Renraku-Help

		and a method extension with the protocol '*renraku'.
		MCPackageManager>>methodModified: should only mark 'Renraku' as modified,
		not 'Renraku-Help'"

	| managers |
	self assert: (MCPackageManager registry includesKey: mcPackage1).
	self assert: (MCPackageManager registry includesKey: mcPackage2).

	managers := OrderedCollection new.
	MCPackageManager managersForCategory: mcPackage1 name do: [ :manager | managers add: manager ].

	self assert: managers size equals: 1.
	self assert: managers first package identicalTo: mcPackage1.

	managers := OrderedCollection new.
	MCPackageManager managersForCategory: mcPackage2 name do: [ :manager | managers add: manager ].

	self assert: managers size equals: 1.
	self assert: managers first package identicalTo: mcPackage2
]
