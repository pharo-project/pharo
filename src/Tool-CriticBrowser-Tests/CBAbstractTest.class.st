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
	#category : #'Tool-CriticBrowser-Tests-Test-Button'
}

{ #category : #testing }
CBAbstractTest class >> isAbstract [ 
	^ self = CBAbstractTest 
]

{ #category : #setUp }
CBAbstractTest >> criticBrowserClass [
	^ CBCriticBrowser
]

{ #category : #setUp }
CBAbstractTest >> selectInstanceVariableCapitalizationRule [
	self selectRuleNamed: #GRInstanceVariableCapitalizationRule
]

{ #category : #setUp }
CBAbstractTest >> selectRuleNamed: aSymbol [
	| indexOfSelectedRule styleRules |
	styleRules := (criticBrowser rulesPresenter roots at: 1) rules.

	styleRules
		detect: [ :rule | rule class uniqueIdentifierName = aSymbol ]
		ifFound: [ :ruleToSelect | indexOfSelectedRule := styleRules indexOf: ruleToSelect ].

	criticBrowser rulesPresenter selectPath: {1 . indexOfSelectedRule}
]

{ #category : #setUp }
CBAbstractTest >> selectUnclassifiedMethodRule [
	self selectRuleNamed: #UnclassifiedMethodsRule
]

{ #category : #running }
CBAbstractTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	| rulesToTest environment |
	super setUp.
	rulesToTest := (ReRuleManager uniqueInstance allRules groupedBy: #group) at: #Style.
	environment := RBPackageEnvironment packageName: 'Tool-CriticBrowser-TestEnvironment'.
	criticBrowser := self criticBrowserClass openOnRules: rulesToTest environment: environment
]

{ #category : #running }
CBAbstractTest >> tearDown [
	criticBrowser withWindowDo: #close.
	super tearDown
]
