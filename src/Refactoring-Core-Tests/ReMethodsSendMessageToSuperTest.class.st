Class {
	#name : 'ReMethodsSendMessageToSuperTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
ReMethodsSendMessageToSuperTest >> model [

	^ RBNamespace onEnvironment: (RBClassEnvironment classes: {
				   MyClassAlpha.
				   MyClassBeta })
]

{ #category : 'tests' }
ReMethodsSendMessageToSuperTest >> testClassDoeNotHaveSubclass [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassBeta;
		subclassesList: { #myClassAlpha }.
		
	" MyClassAlpha is not a subclass of MyClassBeta "
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsSendMessageToSuperTest >> testClassHasSubclass [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReClassHasSubclassesCondition new 
		class: myClassAlpha;
		subclassesList: { #MyClassBeta }.
		
	" MyClassBeta is subclass of MyClassAlpha "
	self assert: cond check
]
