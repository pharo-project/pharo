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
CBAbstractTest >> criticBrowserConstructor [
	^ self instanceOfCriticBrowser
		openOnRule: self rulesToTest
		onEnvironment: self environement
]

{ #category : #setUp }
CBAbstractTest >> environement [
	^ self subclassResponsibility 
]

{ #category : #setUp }
CBAbstractTest >> instanceOfCriticBrowser [
	self subclassResponsibility 
]

{ #category : #setUp }
CBAbstractTest >> rulesToTest [
	^ self subclassResponsibility 
]

{ #category : #running }
CBAbstractTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	criticBrowser := self criticBrowserConstructor.
	
]

{ #category : #UIModification }
CBAbstractTest >> setupCriticBrowser: aCriticBrowser [
	self setupSelectionRulesPresenter: aCriticBrowser.
	self setupSelectionCriticPresenter: aCriticBrowser
]

{ #category : #'action-SourceCodePresenter' }
CBAbstractTest >> setupSelectRBUnclassifiedMethodRule [
	| indexRule rulePresenterTmp |
	indexRule := (rulePresenterTmp := (criticBrowser rulesPresenter roots
		at: 1) rules)
		indexOf:
			(rulePresenterTmp
				detect: [ :e | e isKindOf: RBUnclassifiedMethodsRule ]).
	criticBrowser rulesPresenter selectPath: #(1) , indexRule asArray.
	criticBrowser criticsPresenter selectIndex: 1
]

{ #category : #UIModification }
CBAbstractTest >> setupSelectionCriticPresenter: aCriticBrowser [
	aCriticBrowser criticsPresenter selectIndex: 1
]

{ #category : #UIModification }
CBAbstractTest >> setupSelectionRulesPresenter: aCriticBrowser [
	| indexRule rulePresenterTmp |
	indexRule := (rulePresenterTmp := (aCriticBrowser rulesPresenter
		roots at: 1) rules)
		indexOf:
			(rulePresenterTmp
				detect: [ :e | e isKindOf: GRInstanceVariableCapitalizationRule ]).
	aCriticBrowser rulesPresenter selectPath: #(1) , indexRule asArray
]

{ #category : #running }
CBAbstractTest >> tearDown [
	criticBrowser delete.
	super tearDown
]
