Class {
	#name : #RG2MCClassDefinitionTest,
	#superclass : #RG2MCTest,
	#category : #'Ring2-Tests-Monticello'
}

{ #category : #tests }
RG2MCClassDefinitionTest >> testClassesWithTraits [
	
	"classes without special slots"
	
	Smalltalk allClasses
		select: [ :each | each traits notEmpty or: [ each class traits notEmpty ] ] 
		thenDo: [ :each | self testClass: each ]
	
]

{ #category : #tests }
RG2MCClassDefinitionTest >> testNormalClass [
	
	"a class with instance variables, class variables and class instance variables"
	self testClass: MCMockClassB 
]

{ #category : #tests }
RG2MCClassDefinitionTest >> testNormalKernelClasses [
	
	"classes without special slots"
	
	(#Kernel asPackage definedClasses select: [:each | each isClass and: [each usesSpecialVariables not]]) do: [ :each |
		self testClass: each].
]

{ #category : #tests }
RG2MCClassDefinitionTest >> testSpecialClasses [
	
	"classes without special slots"
	
	(#Kernel asPackage definedClasses select: [:each | each isClass and: [each usesSpecialVariables]]) do: [ :each |
		self testClass: each].
]
