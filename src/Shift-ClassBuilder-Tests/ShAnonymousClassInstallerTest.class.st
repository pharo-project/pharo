Class {
	#name : 'ShAnonymousClassInstallerTest',
	#superclass : 'TestCase',
	#category : 'Shift-ClassBuilder-Tests-Installer',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Installer'
}

{ #category : 'tests' }
ShAnonymousClassInstallerTest >> testSubclasses [

	| aSubClass |
	aSubClass := Smalltalk anonymousClassInstaller make: [ :builder |
		             builder
			             superclass: Point;
			             name: #AnotherPoint ].

	self deny: (Point subclasses includes: aSubClass).
	self assert: aSubClass superclass equals: Point.
	self deny: (Smalltalk hasClassNamed: #AnotherPoint).

	self assert: (self packageOrganizer packageOfClassNamed: aSubClass name) isUndefined
]

{ #category : 'tests' }
ShAnonymousClassInstallerTest >> testSubclassesInEnvironment [

	| newEnvironment aSubClass |
	newEnvironment := self class environment class new.

	aSubClass := Object newAnonymousSubclassInEnvironment: newEnvironment.

	self assert: aSubClass environment identicalTo: newEnvironment
]
