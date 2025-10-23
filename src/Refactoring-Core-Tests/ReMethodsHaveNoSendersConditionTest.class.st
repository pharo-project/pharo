Class {
	#name : 'ReMethodsHaveNoSendersConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsHaveNoSendersConditionTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassAlpha . MyClassBeta})]
]

{ #category : 'tests' }
ReMethodsHaveNoSendersConditionTest >> testMethodWithNoSenders [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReMethodsHaveNoSendersCondition new model: self model;
		classSelectorsMapping: { myClassAlpha -> { #methodWithNoSenders } }.
		
	" the method is not sent by anyone "
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators asSet equals: { myClassAlpha -> (myClassAlpha methodFor: #methodWithNoSenders) } asSet
]

{ #category : 'tests' }
ReMethodsHaveNoSendersConditionTest >> testMethodWithNoSendersWithMessage [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := ReMethodsHaveNoSendersCondition new model: self model;
		classSelectorsMapping: { myClassAlpha -> { #methodWithNoSenders } }.
		
	" the method does not refer to shared variables "
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReMethodsHaveNoSendersConditionTest >> testMethodWithSend [
	| myClassBeta cond violatorSelectors |
	myClassBeta := self model classNamed: #myClassBeta.	
	cond := ReMethodsHaveNoSendersCondition new model: self model;
		classSelectorsMapping: { 
			myClassBeta -> { #methodSentFromAMethodInItsOwnClass } 
		}.
		
	" one of the methods have senders "
	self deny: cond check.
	violatorSelectors := (cond violators collect: [ :assoc | assoc key ]) asArray.
	self assert: violatorSelectors equals: { #methodSentFromAMethodInItsOwnClass }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReMethodsHaveNoSendersConditionTest >> testMethodWithSuperSend [
	| myClassAlpha myClassBeta cond violatorSelectors |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	myClassBeta := self model classNamed: #MyClassBeta.
	cond := ReMethodsHaveNoSendersCondition new model: self model;
		classSelectorsMapping: { 
			myClassAlpha -> { #methodWithSuperSend }
		}.

	" the method is sent from MyClassBeta >> #methodWithSuperSend "
	self deny: cond check.
	violatorSelectors := (cond violators collect: [ :assoc | assoc key ]) asArray.
	self assert: violatorSelectors equals: { #methodWithSuperSend }.
	self assert: cond nonViolators isEmpty
]
