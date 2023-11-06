Class {
	#name : 'MCWorkingCopyManagementTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'package2',
		'package1',
		'mcPackage1',
		'mcPackage2',
		'classToRemove'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MCWorkingCopyManagementTest >> setUp [

	super setUp.
	package1 := self packageOrganizer addPackage: #A , UUID new asString36.
	package2 := self packageOrganizer addPackage: package1 name , #'-SubPart'.
	mcPackage1 := package1 mcPackage.
	mcPackage2 := package2 mcPackage
]

{ #category : 'running' }
MCWorkingCopyManagementTest >> tearDown [

	classToRemove ifNotNil: [ classToRemove removeFromSystem ].

	package1 removeFromSystem.
	package2 removeFromSystem.
	MCWorkingCopy registry
		removeKey: mcPackage1 ifAbsent: [  ];
		removeKey: mcPackage2 ifAbsent: [  ].
	super tearDown
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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
