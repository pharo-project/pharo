Class {
	#name : #RGMCClassTest,
	#superclass : #RGMCTest,
	#category : #'Ring-Tests-Monticello'
}

{ #category : #tests }
RGMCClassTest >> testClassesWithTraits [
	
	"classes without special slots"
	
	self skip. 
	"skip because of https://pharo.fogbugz.com/f/cases/22080/Monticello-wrongly-resolves-names-for-instance-variables-in-classes-that-contain-slots"
	
	Smalltalk allClasses
		select: [ :each | each isTrait not and: [each traits notEmpty or: [ each class traits notEmpty ]] ] 
		thenDo: [ :each | self testClass: each ]
	
]

{ #category : #tests }
RGMCClassTest >> testNormalClass [
	
	"a class with instance variables, class variables and class instance variables"
	self testClass: MCMockClassB 
]

{ #category : #tests }
RGMCClassTest >> testNormalKernelClasses [
	
	"classes without special slots"
	
	(#Kernel asPackage definedClasses select: [:each | each isClass and: [each needsSlotClassDefinition  not]]) do: [ :each |
		self testClass: each].
]

{ #category : #tests }
RGMCClassTest >> testSpecialClasses [
	
	"classes without special slots"
	
	(#Kernel asPackage definedClasses select: [:each | each isClass and: [each needsSlotClassDefinition]]) do: [ :each |
		self testClass: each].
]
