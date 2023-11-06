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

{ #category : 'accessing' }
ShiftClassBuilderAbstractTest >> packageNameForTest [

	^ #FakedCore
]

{ #category : 'running' }
ShiftClassBuilderAbstractTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTest.

	super tearDown
]
