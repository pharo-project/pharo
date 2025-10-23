"
A ReDefinesSelectorsConditionTest is a test class for testing the behavior of ReDefinesSelectorsCondition
"
Class {
	#name : 'ReDefinesSelectorsConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MyClassBetaSub})]
]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDirectlyDefinesSelector [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#accessing . #accessingSharedVariable}  in: myClassARoot.
		
	" the class MyClassARoot defines the methods #accessing and #accessingSharedVariable"
	self assert: def check. 	
	self assert: def violators isEmpty.
	self assert: def nonViolators equals: {#accessing . #accessingSharedVariable}
]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDirectlyDefinesSelectorWithMessage [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#accessing . #accessingSharedVariable}  in: myClassARoot.
		
	" the class MyClassARoot defines the methods #accessing and #accessingSharedVariable"
	self assert: def errorString isEmpty

]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelector [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#accessingZZZ . #accessingSharedVariable}  in: myClassARoot.
		
	" the class MyClassARoot does not define method #accessingZZZ"
	self deny: def check. 	
	self assert: def violators isNotEmpty.
	self assert: def violators size equals: 1.
	self assert: def nonViolators equals: { #accessingSharedVariable }

]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelectorEvenIfSuperClassDoes [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassBetaSub.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#fooAccessor }  in: myClassARoot.
		
	" the class MyClassBetaSub does not define method #fooAccessor"
	self deny: def check. 	
	self assert: def violators isNotEmpty.
	self assert: def violators size equals: 1.
	self assert: def nonViolators isEmpty

]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelectorEvenIfSuperClassDoesWithMessage [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassBetaSub.
	
	def := ReDefinesSelectorsCondition new definesSelectors: { #fooAccessor }  in: myClassARoot.
		
	" the class MyClassBetaSub does not define method #fooAccessor"
	self assert: ('*fooAccessor*' match: def errorString)
]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelectorWithMessage [

	| myClassARoot def defined undefined |
	myClassARoot := self model classNamed: #MyClassARoot.
	undefined := #accessingZZZ.
	defined := #accessingSharedVariable.
	def := ReDefinesSelectorsCondition new definesSelectors: { defined . undefined }  in: myClassARoot.
		
	" the class MyClassARoot does not define method #accessingZZZ"
	self assert: ('*' , undefined , '*' match: def errorString).
	self deny: ('*' , defined , '*' match: def errorString)
]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testMetaClassDirectlyDefinesSelector [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassARoot.
	myClassARoot := myClassARoot classSide.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#accessingFromClassSide}  in: myClassARoot.
		
	" the class MyClassARoot defines the methods #accessing and #accessingSharedVariable"
	self assert: def check. 	
	self assert: def violators isEmpty.
	self assert: def nonViolators equals: { #accessingFromClassSide }

]
