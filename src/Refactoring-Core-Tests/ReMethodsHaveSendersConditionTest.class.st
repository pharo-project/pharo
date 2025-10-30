Class {
	#name : 'ReMethodsHaveSendersConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsHaveSendersConditionTest >> classToTest [
	^ ReMethodsHaveSendersCondition
]

{ #category : 'accessing' }
ReMethodsHaveSendersConditionTest >> model [

	^ model ifNil: [
			  model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
						            MyClassAlpha.
						            MyClassBeta }) ]
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethoHasInternalSendWithNegation [

	| myClassBeta hasSenders noSenders violatorSelectors nonViolatorSelectors |
	myClassBeta := self model classNamed: #MyClassBeta.
	hasSenders := self classToTest new
		             model: self model;
		             classSelectorsMapping:
			             { (myClassBeta -> { 
									#methodSentFromAMethodInItsOwnClass.
									#methodSendingAMethodFromItsOwnClass }) } asDictionary. 
									
	" no method have external senders "
	self deny: hasSenders check.

	violatorSelectors := (hasSenders violators collect: [ :assoc | assoc value selector ]).
	self assert: violatorSelectors asSet equals: { #methodSendingAMethodFromItsOwnClass . #methodSentFromAMethodInItsOwnClass } asSet.
	self assert: hasSenders nonViolators isEmpty.

	noSenders := hasSenders not.
	self assert: noSenders check.
	self assert: noSenders violators isEmpty.	
	nonViolatorSelectors := (noSenders nonViolators collect: [ :assoc | assoc value selector ]).
	self assert: nonViolatorSelectors asSet equals: { #methodSendingAMethodFromItsOwnClass . #methodSentFromAMethodInItsOwnClass } asSet
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasExternalSend [

	| myClassBeta hasSenders nonViolatorSelectors |
	myClassBeta := self model classNamed: #MyClassBeta.
	hasSenders := self classToTest new
		             model: self model;
		             classSelectorsMapping:
			             { (myClassBeta -> { 
									#methodSentFromAMethodInItsOwnClass }) } asDictionary. 

	" method has external senders "
	self assert: hasSenders check.

	self assert: hasSenders violators isEmpty.
	nonViolatorSelectors := (hasSenders nonViolators collect: [ :assoc | assoc value selector ]) asArray.
	self assert: nonViolatorSelectors equals: { #methodSentFromAMethodInItsOwnClass }

]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasExternalSuperSend [
	| myClassAlpha myClassBeta cond nonViolatorSelectors |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	myClassBeta := self model classNamed: #MyClassBeta.
	cond := self classToTest new model: self model;
		classSelectorsMapping: { 
			myClassAlpha -> { #methodWithSuperSend }
		} asDictionary.

	" the method is sent from the external method MyClassBeta >> #methodWithSuperSend "
	self assert: cond check.
	self assert: cond violators isEmpty.
	nonViolatorSelectors := (cond nonViolators collect: [ :assoc | assoc value selector ]) asArray.
	self assert: nonViolatorSelectors equals: { #methodWithSuperSend }
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasInternalSend [

	| myClassBeta hasSenders violatorSelectors |
	myClassBeta := self model classNamed: #MyClassBeta.
	hasSenders := self classToTest new
		             model: self model;
		             classSelectorsMapping:
			             { (myClassBeta -> { 
									#methodSentFromAMethodInItsOwnClass.
									#methodSendingAMethodFromItsOwnClass }) } asDictionary. 

	" no method has external senders "
	self deny: hasSenders check.
	self assert: hasSenders nonViolators isEmpty.
	violatorSelectors := (hasSenders violators collect: [ :assoc | assoc value selector ]) asArray.
	self assert: violatorSelectors asSet equals: { #methodSentFromAMethodInItsOwnClass . #methodSendingAMethodFromItsOwnClass } asSet

]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasInternalSuperSend [
	| myClassAlpha myClassBeta cond violatorClasses |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	myClassBeta := self model classNamed: #MyClassBeta.
	cond := self classToTest new model: self model;
		classSelectorsMapping: { 
			myClassAlpha -> { #methodWithSuperSend }.
			myClassBeta -> { #methodWithSuperSend }
		} asDictionary.

	" the method is sent from the internat method MyClassBeta >> #methodWithSuperSend "
	self deny: cond check.
	violatorClasses := (cond violators collect: [ :assoc | assoc key ]).
	self assert: violatorClasses asSet equals: {myClassBeta . myClassAlpha} asSet.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasNoSenders [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := self classToTest new model: self model;
		classSelectorsMapping: { myClassAlpha -> { #methodWithNoSenders } } asDictionary .
	
	" the method is not sent by anyone "
	self deny: cond check.
	self assert: cond nonViolators isEmpty.
	self assert: (cond violators collect: [ :each | each value selector ]) equals: { #methodWithNoSenders } asSet
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasNoSendersMessage [

	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.

	cond := self classToTest new
		        model: self model;
		        classSelectorsMapping: { (myClassAlpha -> { #methodWithNoSenders }) } asDictionary. " the method does not refer to shared variables "
	self
		assert: cond errorString
		equals: 'Method(s) don''t have senders: rb-MyClassAlpha->rb-MyClassAlpha>>methodWithNoSenders; '
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testMethodHasNoSendersWithNegation [
	| myClassAlpha cond |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	
	cond := self classToTest new model: self model;
		classSelectorsMapping: { myClassAlpha -> { #methodWithNoSenders } } asDictionary .
	
	" the method is not sent by anyone "
	self deny: cond check.
	self assert: cond nonViolators isEmpty.
	self assert: (cond violators collect: [ :each | each value selector ]) equals: { #methodWithNoSenders } asSet.
	cond := cond not.
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: (cond nonViolators collect: [ :each | each value selector ]) equals: { #methodWithNoSenders } asSet
]

{ #category : 'tests' }
ReMethodsHaveSendersConditionTest >> testSomeMethodsHaveExternalSend [
	| myClassAlpha myClassBeta cond violatorClasses violatorSelectors nonViolators |
	myClassAlpha := self model classNamed: #MyClassAlpha.
	myClassBeta := self model classNamed: #MyClassBeta.
	cond := self classToTest new model: self model;
		classSelectorsMapping: { 
			myClassBeta -> { 
				#methodSentFromAMethodInItsOwnClass ."Has an external send"
				#methodWithSuperSend "Has no send"
			}.
			myClassAlpha -> { #methodWithSuperSend }. "Has an internal send" "Has no send"
		} asDictionary.

	" Not every method has an external send "
	self deny: cond check.
	violatorClasses := (cond violators collect: [ :assoc | assoc key ]).
	violatorSelectors := (cond violators collect: [ :assoc | assoc value selector ]).
	self assert: cond violators size equals: 2.
	self assert: violatorClasses asSet equals: {myClassBeta . myClassAlpha} asSet.
	self assert: violatorSelectors equals: { #methodWithSuperSend } asSet.
	
	nonViolators := cond nonViolators.
	self assert: nonViolators size equals: 1.
	self assert: nonViolators anyOne value selector equals: #methodSentFromAMethodInItsOwnClass
]
