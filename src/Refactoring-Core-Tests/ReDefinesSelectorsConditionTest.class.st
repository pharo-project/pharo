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

]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelector [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#accessingZZZ . #accessingSharedVariable}  in: myClassARoot.
		
	" the class MyClassARoot does not define method #accessingZZZ"
	self deny: def check. 	
	self assert: def violators isNotEmpty.
	self assert: def violators size equals: 1

]

{ #category : 'accessing' }
ReDefinesSelectorsConditionTest >> testClassDoesNotDirectlyDefinesSelectorEvenIfSuperClassDoes [

	| myClassARoot def |
	myClassARoot := self model classNamed: #MyClassBetaSub.
	
	def := ReDefinesSelectorsCondition new definesSelectors: {#fooAccessor }  in: myClassARoot.
		
	" the class MyClassBetaSub does not define method #fooAccessor"
	self deny: def check. 	
	self assert: def violators isNotEmpty.
	self assert: def violators size equals: 1

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

]
