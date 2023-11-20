Class {
	#name : 'AbstractClassDescriptionTest',
	#superclass : 'TestCase',
	#instVars : [
		'class'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'helpers' }
AbstractClassDescriptionTest >> classNameForTests [

	^ #ClassDescriptionGeneratedTestClass
]

{ #category : 'helpers' }
AbstractClassDescriptionTest >> packageNameForTests [

	^ #'Generated-Kernel-Test-Package'
]

{ #category : 'running' }
AbstractClassDescriptionTest >> setUp [

	super setUp.
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: self classNameForTests;
			         package: self packageNameForTests ]
]

{ #category : 'running' }
AbstractClassDescriptionTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	super tearDown
]
