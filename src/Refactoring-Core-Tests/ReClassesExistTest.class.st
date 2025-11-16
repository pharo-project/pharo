Class {
	#name : 'ReClassesExistTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesExistTest >> testClassDoesNotExist [
	| nonexistent cond |
	nonexistent := #Imaginary.
	cond := ReClassesExistCondition new 
		classes: { nonexistent -> nil } asDictionary.
	
	self deny: cond check.
	self assert: cond violators equals: { nonexistent }
]

{ #category : 'tests' }
ReClassesExistTest >> testClassDoesNotExistWithMessage [
	| nonexistent cond |
	nonexistent := #Imaginary.
	cond := ReClassesExistCondition new 
		classes: { nonexistent -> nil } asDictionary.

	self assert: ('*' , nonexistent , '*' match: cond errorString).
]

{ #category : 'tests' }
ReClassesExistTest >> testClassExists [
	| existing existingClass cond |
	existing := #MyClassAlpha.
	existingClass := self model classNamed: existing.
	
	cond := ReClassesExistCondition new 
		classes: { existing -> existingClass } asDictionary.
		
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { existing }
]

{ #category : 'tests' }
ReClassesExistTest >> testClassExistsWithMessage [
	| existing cond existingClass |
	existing := #MyClassAlpha.
	existingClass := self model classNamed: existing.
	
	cond := ReClassesExistCondition new 
		classes: { existing -> existingClass } asDictionary.
	
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReClassesExistTest >> testSomeClassesDoNotExist [

	| nonexistent existing existingClass cond |
	nonexistent := #Imaginary.
	existing := #Object.
	existingClass := self model classNamed: existing.

	cond := ReClassesExistCondition new classes: {
			        (nonexistent -> nil).
			        (existing -> existingClass) } asDictionary.

	self deny: cond check.
	self assert: cond violators equals: { nonexistent }.
	self assert: cond nonViolators equals: { existing }
]

{ #category : 'tests' }
ReClassesExistTest >> testSomeClassesDoNotExistWithMessage [

	| nonexistent existing existingClass cond |
	nonexistent := #Imaginary.
	existing := #Object.
	existingClass := self model classNamed: existing.

	cond := ReClassesExistCondition new classes: {
			        (nonexistent -> nil).
			        (existing -> existingClass) } asDictionary.

	self assert: ('*' , nonexistent , '*' match: cond errorString).
	self deny: ('*' , existing , '*' match: cond errorString).
]
