"
i'm an abstract class 
my subclass define:
	* environement : return an environement <RBBrowserEnvironment> 
	* instanceOfCriticBrowser : return a class <CriticBrowser> 
		if you want to use MockClas
	* rulesToTest : return an Array of <ReAbstractRule>
	
i define a template method:
	criticBrowserConstructor : return an instance of <CriticBrowser>
"
Class {
	#name : #CBAbstractTest,
	#superclass : #TestCase,
	#instVars : [
		'criticBrowser'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #testing }
CBAbstractTest class >> isAbstract [ 
	^ self = CBAbstractTest 
]

{ #category : #setUp }
CBAbstractTest >> newCriticBrowser [
	| rulesToTest environment |
	rulesToTest := (ReRuleManager uniqueInstance allRules groupedBy: #group) at: #Style.
	environment := RBPackageEnvironment packageName: 'Tool-CriticBrowser-TestEnvironment'.
	^ CBCriticBrowser openOnRules: rulesToTest environment: environment
]

{ #category : #setUp }
CBAbstractTest >> selectInstanceVariableCapitalizationRule [
	self selectRuleNamed: #GRInstanceVariableCapitalizationRule
]

{ #category : #setUp }
CBAbstractTest >> selectRuleNamed: aSymbol [
	| indexOfSelectedRule |
	self styleRules
		detect: [ :rule | rule class uniqueIdentifierName = aSymbol ]
		ifFound: [ :ruleToSelect | indexOfSelectedRule := self styleRules indexOf: ruleToSelect ].

	criticBrowser rulesPresenter selectPath: {1 . indexOfSelectedRule}
]

{ #category : #setUp }
CBAbstractTest >> selectUnclassifiedMethodRule [
	self selectRuleNamed: #UnclassifiedMethodsRule
]

{ #category : #running }
CBAbstractTest >> setUp [
	super setUp.
	criticBrowser := self newCriticBrowser 
]

{ #category : #setUp }
CBAbstractTest >> styleRules [
	^ (criticBrowser rulesPresenter roots at: 1) rules
]

{ #category : #running }
CBAbstractTest >> tearDown [
	criticBrowser withWindowDo: #close.
	super tearDown
]
