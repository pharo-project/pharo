Class {
	#name : #ShAnonymousClassInstallerTest,
	#superclass : #TestCase,
	#category : #'Shift-ClassInstaller-Tests'
}

{ #category : #tests }
ShAnonymousClassInstallerTest >> testSubclasses [

	| aSubClass |
	aSubClass := Smalltalk anonymousClassInstaller make: [ :builder |
		             builder superclass: Point.
		             builder name: #AnotherPoint ].

	self deny: (Point subclasses includes: aSubClass).
	self assert: aSubClass superclass equals: Point.
	self deny: (Smalltalk hasClassNamed: #AnotherPoint).

	self assert: (Smalltalk organization packageOfClassNamed: #AnotherPoint) equals: nil
]
