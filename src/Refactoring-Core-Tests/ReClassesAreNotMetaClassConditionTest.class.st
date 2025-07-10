"
A ReClassesAreNotMetaClassConditionTest is a test class for testing the behavior of ReClassesAreNotMetaClassCondition
"
Class {
	#name : 'ReClassesAreNotMetaClassConditionTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassesAreNotMetaClassConditionTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState} )
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassConditionTest >> testClassIsAMetaclass [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }.
		
	self deny: cond check. 
	self deny: cond violators isEmpty.
	
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassConditionTest >> testClassIsAMetaclassWithMessages [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }; message: 'Manipulate classes and not metaclasses'.
		
	self deny: cond check. 
	self assert: (String streamContents: [:s | cond violationMessageOn: s] )
		equals:  'MyClassARoot is a meta class. Manipulate classes and not metaclasses'
	
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassConditionTest >> testClassIsAMetaclassWithoutMessages [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot classSide }.
		
	self deny: cond check. 
	self assert: (String streamContents: [:s | cond violationMessageOn: s] )
		equals:  'MyClassARoot is a meta class.'
	
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassConditionTest >> testClassIsNotAMetaclass [

	| myClassARoot cond |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	cond := ReClassesAreNotMetaClassCondition new 
		classes: { myClassARoot }.
		
	self assert: cond check. 
	self assert: cond violators isEmpty.
	
]
