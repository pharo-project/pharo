Class {
	#name : 'ReMethodsReceiveNoSupersendsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'accessing' }
ReMethodsReceiveNoSupersendsTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment:
			           (RBClassEnvironment classes: {
					            MyClassBeta.
					            MyClassBetaSibling.
					            MyClassAlpha }) ]
]

{ #category : 'tests' }
ReMethodsReceiveNoSupersendsTest >> testMethodThatIsNeverSentToSuper [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsReceiveNoSupersendsCondition new
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
	
	cond := ReMethodsReceiveNoSupersendsCondition new
			class: myClassBeta 
		   targetSuperclass: myClassAlpha 
		   selectors: { #methodForSupersend }.
	
	" a method in MyClassBetaSibling sends `methodForSupersend` to `super`, therefore the condition fails "
	self deny: cond check
]
