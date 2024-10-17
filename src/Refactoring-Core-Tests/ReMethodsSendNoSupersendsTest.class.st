Class {
	#name : 'ReMethodsSendNoSupersendsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsSendNoSupersendsTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment:
			           (RBClassEnvironment classes: {
					            MyClassBeta.
					            MyClassAlpha }) ]
]

{ #category : 'tests' }
ReMethodsSendNoSupersendsTest >> testMethodNotSendingAMessageToSuper [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReNoSupersendToTargetClassCondition new
			class: myClassBeta
		   targetSuperclass: myClassAlpha
		   selectors: { #methodForPullUp }.
		
	"this is a simple method without any super sends, so we are good"
	self assert: cond check
]

{ #category : 'tests' }
ReMethodsSendNoSupersendsTest >> testMethodSendingMessageToSuper [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReNoSupersendToTargetClassCondition new
			class: myClassBeta
		   targetSuperclass: myClassAlpha
		   selectors: { #methodWithSuperSend }.

	"methodWithSuperSend sends a super to target superclass, if we would pull this up, behavior may change"
	self deny: cond check
]
