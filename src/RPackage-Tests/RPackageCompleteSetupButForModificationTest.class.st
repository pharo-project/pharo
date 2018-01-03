"
SUnit tests for RPackage
"
Class {
	#name : #RPackageCompleteSetupButForModificationTest,
	#superclass : #RPackageIncrementalTest,
	#instVars : [
		'p1',
		'p2',
		'p3',
		'a1',
		'b1',
		'a2',
		'a3',
		'b2'
	],
	#category : #'RPackage-Tests'
}

{ #category : #running }
RPackageCompleteSetupButForModificationTest >> setUp [
	super setUp.
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	
	a1 :=  self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	a2 := self createNewClassNamed: #A2DefinedInP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2DefinedInP2 inPackage: p2.
	a3 := self createNewClassNamed: #A3DefinedInP3 inPackage: p3.
	
	a1 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#methodDefinedInP1).
	a1 compileSilently: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p1 addMethod: (a1>>#anotherMethodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: (a2 class>>#classSideMethodDefinedInP3)


]

{ #category : #'test addition' }
RPackageCompleteSetupButForModificationTest >> testAddMethod [
		
	self assert: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	
	a1 compileSilently: 'zork ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#zork).
	self assert: (p1 includesSelector: #zork ofClass: a1).
	
	self assert: (p3 includesSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
	
]

{ #category : #'test addition' }
RPackageCompleteSetupButForModificationTest >> testBasicRemoveClass [
	"we remove a class we check that it is not in the package anymore"

	|  size |
	size := p1 definedClasses size. 
	self assert: size = 2.
	self assert: (p1 includesClass: a1).
	self assert: (p1 includesClass: a1 class).
	p1 removeClassDefinition: a1.
	self assert: p1 definedClasses size = (size - 1).
	self assert: (p1 includesClass: b1).
	self assert: (p1 includesClass: b1 class).
	self deny: (p1 includesClass: a1).
	self deny: (p1 includesClass: a1 class).
	p1 removeClassDefinition: b1.
	self assert: p1 definedClasses size = (size - 2).
	self deny: (p1 includesClass: b1).
	self deny: (p1 includesClass: b1 class).


]

{ #category : #'test starting situation' }
RPackageCompleteSetupButForModificationTest >> testExtensionMethodOfClass [
	"P1 
		A1DefinedInP1>>methodDefinedInP1
		B1DefinedInP1	
		A2DefinedInP2>>methodDefinedInP1  (was *P2)
	
	P2
		A2DefinedInP2>>methodDefinedInP2
		B2DefinedInB2	

	P3
		A3DefinedInP3	
		A2DefinedInP2>>methodDefinedInP3  (was *P2*)"
		
	self assert: a2 selectors size = 3.
	"the class contains methods defined by several packages 
	however when looking with a package view we only see the package 
	extension."
		
	self assert: (p2 extensionMethodsForClass: a2) size = 0.
	self assert: (p1 extensionMethodsForClass: a1) size = 0.
	"show that extensionMethodsForClass: does works for methods of class
	defined in the package"
	
	self assert: (p1 extensionMethodsForClass: a2) size = 1.
	self assert: (p3 extensionMethodsForClass: a2) size = 1.
]

{ #category : #'test addition' }
RPackageCompleteSetupButForModificationTest >> testRemoveMethod [

	p1 removeMethod: a1 >> #methodDefinedInP1.
	self deny: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	
]

{ #category : #'test addition' }
RPackageCompleteSetupButForModificationTest >> testRemoveMethodDoesNotRemoveFromClass [
	"we remove a method so it is not in the package but also not from the class anymore"

	p1 removeMethod:  a1 >> #methodDefinedInP1.
	self deny: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	self assert: (a1 selectors includes: #methodDefinedInP1)
]

{ #category : #'test addition' }
RPackageCompleteSetupButForModificationTest >> testRemoveShouldRaiseErrorWhenMethodIsNotInPackage [
	
	self should: [p2 removeMethod: a2>>#methodDefinedZork] raise: Error
]

{ #category : #'test starting situation' }
RPackageCompleteSetupButForModificationTest >> testStartingSituation [

	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"
	
	self assert: (p2 includesClass: a2).  
	"a locally defined class extended by other packages"
	
	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2).
]
