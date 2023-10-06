Class {
	#name : 'ShiftClassBuilderAbstractTest',
	#superclass : 'TestCase',
	#instVars : [
		'builder'
	],
	#category : 'Shift-ClassBuilder-Tests',
	#package : 'Shift-ClassBuilder-Tests'
}

{ #category : 'accessing' }
ShiftClassBuilderAbstractTest >> builder [

	^ builder
]

{ #category : 'private' }
ShiftClassBuilderAbstractTest >> categoryHack [
	"Currently there is a bug in Fluid class builder that is that when we define a tag X with a package Y it can generate a package Y-X instead. 
	This is due to the category mess in RPackage and it will not be fixed until the category mess is cleaned. Until then, to make sure the tests are in a right state, we register the package without the tag before creating the class to ensure the tag is not added as part of the package name.
	This is ugly but it is currently hard to do better."

	self packageOrganizer ensurePackage: self packageNameForTest
]

{ #category : 'accessing' }
ShiftClassBuilderAbstractTest >> packageNameForTest [

	^ #FakedCore
]

{ #category : 'running' }
ShiftClassBuilderAbstractTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTest.

	super tearDown
]
