"
A ReClassesAreNotMetaClassConditionTest is a test class for testing the behavior of ReClassesAreNotMetaClassCondition
"
Class {
	#name : 'ReClassesAreNotMetaClassTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesAreNotMetaClassTest >> testClassIsAMetaclass [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }.
		
	self deny: cond check. 
	self deny: cond violators isEmpty.
	self assert: cond nonViolators isEmpty
	
]

{ #category : 'tests' }
ReClassesAreNotMetaClassTest >> testClassIsAMetaclassWithMessages [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }; message: 'Manipulate classes and not metaclasses'.
		
	self deny: cond check. 
	self assert: (String streamContents: [:s | cond violationMessageOn: s] )
		equals:  'MyClassARoot is a meta class. Manipulate classes and not metaclasses'
	
]

{ #category : 'tests' }
ReClassesAreNotMetaClassTest >> testClassIsAMetaclassWithoutMessages [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }.
		
	self deny: cond check. 
	self assert: (String streamContents: [:s | cond violationMessageOn: s] )
		equals:  'MyClassARoot is a meta class.'
	
]

{ #category : 'tests' }
ReClassesAreNotMetaClassTest >> testClassIsNotAMetaclass [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot }.
		
	self assert: cond check. 
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { myClassARoot }
	
]
