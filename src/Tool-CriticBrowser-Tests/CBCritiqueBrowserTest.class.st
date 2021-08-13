"
A CBCritiqueBrowserTest is a test class for testing the behaivor of CBCritiqueBrowser
"
Class {
	#name : #CBCritiqueBrowserTest,
	#superclass : #TestCase,
	#instVars : [
		'critiqueBrowser',
		'rules'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #initialization }
CBCritiqueBrowserTest >> classToTest [

	^ CBCritiqueBrowser
]

{ #category : #running }
CBCritiqueBrowserTest >> setUp [

	super setUp.
	critiqueBrowser := CBCritiqueBrowser new.
	rules := (ReRuleManager visibleRuleClasses collect: [ :aClass | 
		          aClass new ]) sorted: [ :a :b | a name < b name ].
]

{ #category : #initialization }
CBCritiqueBrowserTest >> testBrowseRule [

	| windowBrowser singleRule |
	singleRule := rules first.
	critiqueBrowser rules: rules.
	critiqueBrowser rulesModel selectItem: singleRule.
	windowBrowser := critiqueBrowser browseRule.
	self assert: windowBrowser class equals: ClyFullBrowserMorph.
	windowBrowser close.
	
]

{ #category : #initialization }
CBCritiqueBrowserTest >> testInitializePresenters [

	self
		assert: critiqueBrowser rulesModel class
		equals: SpTreeTablePresenter.
	self
		assert: critiqueBrowser sourceCodeModel class
		equals: SpCodePresenter.
	self
		assert: critiqueBrowser toolbarModel class
		equals: CBCritiqueToolbar.
	self
		assert: critiqueBrowser criticsModel class
		equals: SpListPresenter.

		
]

{ #category : #initialization }
CBCritiqueBrowserTest >> testOpen [

	| window |
	window := self classToTest open.
	self assert: window isBuilt.
	window close
]
