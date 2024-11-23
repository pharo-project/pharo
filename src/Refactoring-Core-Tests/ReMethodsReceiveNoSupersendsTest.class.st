Class {
	#name : 'ReMethodsReceiveNoSupersendsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsReceiveNoSupersendsTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment:
			           (RBClassEnvironment classes: {
					            MyClassBeta.
					            MyClassBetaSibling.
								   MyClassBetaSub.
					            MyClassAlpha }) ]
]

{ #category : 'tests' }
ReMethodsReceiveNoSupersendsTest >> testMethodThatIsNeverSentToSuper [
	"Checks that there is no super call to a method being pulled up."
	
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: myClassBeta 
		   targetSuperclass: myClassAlpha 
		   selectors: { #methodForPullUp }.
	
	" the method `methodForPullUp` is never sent to `super`, therefore the condition succeeds "
	self assert: cond check
]

{ #category : 'tests' }
ReMethodsReceiveNoSupersendsTest >> testMethodThatIsSentToSuperFromASiblingsMethod [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: myClassBeta 
		   targetSuperclass: myClassAlpha 
		   selectors: { #methodForSupersend }.
	
	" a method in MyClassBetaSibling sends `methodForSupersend` to `super`, therefore the condition fails "
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsReceiveNoSupersendsTest >> testMethodThatIsSentToSuperInLeafFromASiblingsMethod [
	
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBetaSibling.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: myClassBeta 
		   targetSuperclass: myClassAlpha 
		   selectors: { #methodForSupersendInLeaf }.
	
	" a method in MyClassBetaSibling sends `methodForSupersend` to `super`, therefore the condition fails "
	self deny: cond check
]
