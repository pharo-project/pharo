Class {
	#name : 'ReMethodsSendNoSupersendsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
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
	
	cond := ReMethodsSendNoSupersendsCondition new
			class: myClassBeta
		   targetSuperclass: myClassAlpha
		   selectors: { #methodForPullUp }.
		
	" the method has a duplicate in MyClassBeta's sibling (MyClassBetaSibling), therefore the condition fails "
	self assert: cond check
]

{ #category : 'tests' }
ReMethodsSendNoSupersendsTest >> testMethodSendingMessageToSuper [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsSendNoSupersendsCondition new
			class: myClassBeta
		   targetSuperclass: myClassAlpha
		   selectors: { #methodWithSuperSend }.
		
	" the method has a duplicate in MyClassBeta's sibling (MyClassBetaSibling), therefore the condition fails "
	self deny: cond check
]
