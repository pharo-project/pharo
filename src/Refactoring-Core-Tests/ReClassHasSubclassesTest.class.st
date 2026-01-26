Class {
	#name : 'ReClassHasSubclassesTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassHasSubclassesTest >> testClassDoesNotHaveSubclass [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassBeta;
		subclassesList: { #myClassAlpha . #Object }.
		
	" MyClassAlpha is not a subclass of MyClassBeta "
	self deny: cond check.
	self assert: cond violators equals: { myClassBeta }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReClassHasSubclassesTest >> testClassHasSubclass [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassAlpha;
		subclassesList: { #MyClassBeta . #MyClassBetaSibling }.
		
	" MyClassBeta and MyClassBetaSibling ares subclasses of MyClassAlpha "
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { myClassAlpha }
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
