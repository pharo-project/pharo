Class {
	#name : 'ReClassHasSubclassesTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassHasSubclassesTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
				   MyClassAlpha.
				   MyClassBeta.
				   MyClassBetaSibling.
				   Object })]
]

{ #category : 'tests' }
ReClassHasSubclassesTest >> testClassDoesNotHaveSubclass [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassBeta;
		subclassesList: { #myClassAlpha . #Object }.
		
	" MyClassAlpha is not a subclass of MyClassBeta "
	self deny: cond check
]

{ #category : 'tests' }
ReClassHasSubclassesTest >> testClassHasSubclass [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassAlpha;
		subclassesList: { #MyClassBeta . #MyClassBetaSibling }.
		
	" MyClassBeta and MyClassBetaSibling ares subclasses of MyClassAlpha "
	self assert: cond check
]

{ #category : 'tests' }
ReClassHasSubclassesTest >> testOnlySomeElementsOfTheListAreSubclasses [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassAlpha;
		subclassesList: { #myClassBeta . #Object }.
		
	" Not all elements of the subclassesList are subclass of MyClassAlpha "
	self deny: cond check
]
