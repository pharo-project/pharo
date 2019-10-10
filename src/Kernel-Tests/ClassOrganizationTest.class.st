"
SUnit tests for class organization
"
Class {
	#name : #ClassOrganizationTest,
	#superclass : #TestCase,
	#instVars : [
		'organization',
		'class'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #accessing }
ClassOrganizationTest >> organization [ 
	^ organization
]

{ #category : #running }
ClassOrganizationTest >> runCase [ 
	SystemAnnouncer uniqueInstance 
		suspendAllWhile: [ super runCase ]
]

{ #category : #running }
ClassOrganizationTest >> setUp [ 
	super setUp.
	class := Object subclass: #ClassForTests instanceVariableNames: '' classVariableNames: '' category: 'ClassOrganizer-Tests'.
	organization := ClassOrganization forClass: class.
	organization addCategory: 'empty'.
	organization addCategory: 'one'.
	organization classify: #one under: 'one' suppressIfDefault: true.
	
]

{ #category : #running }
ClassOrganizationTest >> tearDown [
	class removeFromSystem.
	super tearDown.
]

{ #category : #tests }
ClassOrganizationTest >> testAddCategory [
	
	self organization addCategory: 'test-protocol'.
	
	self assert: (self organization categories includes: 'test-protocol')
]

{ #category : #tests }
ClassOrganizationTest >> testCategories [
	| categories |
	
	categories := self organization categories.
	self assert: categories notEmpty.
]

{ #category : #tests }
ClassOrganizationTest >> testListAtCategoryNamed [
	| methods |
	methods := self organization listAtCategoryNamed: 'empty'.
	self assertEmpty: methods.

	methods := self organization listAtCategoryNamed: 'one'.
	self assert: methods size equals: 1.
	self assert: methods first equals: #one
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveCategory [
	self assert: self organization categories size equals: 2.
	self should: [ self organization removeCategory: 'one' ] raise: Error.
	self organization removeCategory: 'empty'.
	self assert: self organization categories size equals: 1.
	self assert: self organization categories first equals: 'one'
]

{ #category : #tests }
ClassOrganizationTest >> testRemoveNonExistingProtocol [
	self organization removeCategory: 'non-existent'
]
