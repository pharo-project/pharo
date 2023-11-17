"
SUnit tests for RPackage
"
Class {
	#name : 'RPackageCompleteSetupButForModificationTest',
	#superclass : 'RPackageIncrementalTest',
	#instVars : [
		'a1',
		'b1',
		'a2',
		'b2',
		'xPackage',
		'yPackage',
		'zPackage'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'running' }
RPackageCompleteSetupButForModificationTest >> setUp [

	super setUp.
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	a1 := self newClassNamed: #A1DefinedInX in: xPackage.
	b1 := self newClassNamed: #B1DefinedInX in: xPackage.
	a2 := self newClassNamed: #A2DefinedInY in: yPackage.
	b2 := self newClassNamed: #B2DefinedInY in: yPackage.

	a1 compile: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.

	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.
	a2 class compile: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3' classified: '*' , zPackage name
]

{ #category : 'test - addition' }
RPackageCompleteSetupButForModificationTest >> testAddMethod [

	self assert: (xPackage includesSelector: #methodDefinedInP1 ofClass: a1).

	a1 compileSilently: 'zork ^ #methodDefinedInP1'.
	xPackage addMethod: (a1>>#zork).
	self assert: (xPackage includesSelector: #zork ofClass: a1).

	self assert: (zPackage includesSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
]

{ #category : 'test - addition' }
RPackageCompleteSetupButForModificationTest >> testBasicRemoveClass [
	"we remove a class we check that it is not in the package anymore"

	|  size |
	size := xPackage definedClasses size.
	self assert: size equals: 2.
	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: a1 class).
	xPackage removeClass: a1.
	self assert: xPackage definedClasses size equals: (size - 1).
	self assert: (xPackage includesClass: b1).
	self assert: (xPackage includesClass: b1 class).
	self deny: (xPackage includesClass: a1).
	self deny: (xPackage includesClass: a1 class).
	xPackage removeClass: b1.
	self assert: xPackage definedClasses size equals: (size - 2).
	self deny: (xPackage includesClass: b1).
	self deny: (xPackage includesClass: b1 class)
]

{ #category : 'test - starting situation' }
RPackageCompleteSetupButForModificationTest >> testExtensionMethodOfClass [
	"P1
		A1DefinedInX>>methodDefinedInP1
		B1DefinedInX
		A2DefinedInY>>methodDefinedInP1  (was *P2)

	P2
		A2DefinedInY>>methodDefinedInP2
		B2DefinedInB2

	P3
		A3DefinedInZ
		A2DefinedInY>>methodDefinedInP3  (was *P2*)"

	self assert: a2 selectors size equals: 3.
	"the class contains methods defined by several packages
	however when looking with a package view we only see the package
	extension."

	self assert: (yPackage extensionMethodsForClass: a2) size equals: 0.
	self assert: (xPackage extensionMethodsForClass: a1) size equals: 0.
	"show that extensionMethodsForClass: does works for methods of class
	defined in the package"

	self assert: (xPackage extensionMethodsForClass: a2) size equals: 1.
	self assert: (zPackage extensionMethodsForClass: a2) size equals: 1
]

{ #category : 'test - addition' }
RPackageCompleteSetupButForModificationTest >> testRemoveMethod [

	a1 removeSelector: #methodDefinedInP1.
	self deny: (xPackage includesSelector: #methodDefinedInP1 ofClass: a1)
]

{ #category : 'test - addition' }
RPackageCompleteSetupButForModificationTest >> testRemoveShouldRaiseErrorWhenMethodIsNotInPackage [

	self should: [ yPackage removeMethod: a2>>#methodDefinedZork ] raise: Error
]

{ #category : 'test - starting situation' }
RPackageCompleteSetupButForModificationTest >> testStartingSituation [

	self deny: (yPackage includesClass: b1).
	self assert: (yPackage includesClass: b2).
	"a locally defined class not extended by other packages"

	self assert: (yPackage includesClass: a2).
	"a locally defined class extended by other packages"

	self assert: (xPackage definesOrExtendsClass: a2).
	self deny: (xPackage includesClass: a2)
]
