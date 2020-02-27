Class {
	#name : #MCPackageManagerTest,
	#superclass : #MCTestCase,
	#instVars : [
		'package2',
		'package1',
		'mcPackage1',
		'mcPackage2'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #running }
MCPackageManagerTest >> setUp [
	super setUp.
	package1 := (RPackage named: #A, UUID new asString36) register.
	package2 := (RPackage named: package1 name, #'-SubPart') register.
	mcPackage1 := MCPackage named: package1 name.
	mcPackage2 := MCPackage named: package2 name.
	"register MC packages"
	MCPackageManager
		forPackage: mcPackage1;
		forPackage: mcPackage2
]

{ #category : #running }
MCPackageManagerTest >> tearDown [
	
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
	
	Object subclass: 'A', UUID new asString36 instanceVariableNames: '' classVariableNames: '' category: package2 name.
	
	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified
]

{ #category : #tests }
MCPackageManagerTest >> testClassRemoved [
	
	| class |

	class := Object subclass: 'A', UUID new asString36 instanceVariableNames: '' classVariableNames: '' category: package2 name.
	
	package1 mcWorkingCopy modified: false.
	package2 mcWorkingCopy modified: false.
	
	class removeFromSystem.
	
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
