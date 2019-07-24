Class {
	#name : #CBCriticBrowserTest,
	#superclass : #CBAbstractTest,
	#category : #'Tool-CriticBrowser-Tests-Test-CriticBrowser'
}

{ #category : #'action-CriticsPresenter' }
CBCriticBrowserTest >> testCriticsPresenterClickACriticsShouldTransmitLocationCriticsToSourceCodePresenter [
	"this test check if he transmit the location of the critics and the range "

	self selectInstanceVariableCapitalizationRule.
	criticBrowser criticsPresenter selectIndex: 1.
	self assert: criticBrowser noteBookPresenter selectedPageIndex equals: 2.
	self
		assert: criticBrowser noteBookPageCriticErrorPresenter text
		equals:
			'UseForTestCriticBrowser removeInstVarName: #A!
UseForTestCriticBrowser addInstVarName: #a!
'
]

{ #category : #initialize }
CBCriticBrowserTest >> testCriticsPresenterEmpty [
	
	self assert: criticBrowser criticsPresenter items isEmpty 
]

{ #category : #'as yet unclassified' }
CBCriticBrowserTest >> testDoubleOnCriticShouldSendSourceCodeToSourceCodePresenter [
	| textBefore |
	self selectUnclassifiedMethodRule.
	criticBrowser criticsPresenter selectIndex: 1.
	textBefore := criticBrowser noteBookPageCriticErrorPresenter text.
	self selectInstanceVariableCapitalizationRule.
	criticBrowser criticsPresenter selectIndex: 2.
	self selectUnclassifiedMethodRule.
	"simulate double click"
	criticBrowser criticsPresenter selectIndex: 1.
	criticBrowser criticsPresenter selectIndex: 1.
	self
		assert: criticBrowser noteBookPageCriticErrorPresenter text
		equals: textBefore
]

{ #category : #initialize }
CBCriticBrowserTest >> testInitCriticErrorPresenterIsEmpty [
	self assert: criticBrowser noteBookPageCriticErrorPresenter text equals: ''
]

{ #category : #initialize }
CBCriticBrowserTest >> testInitRuleCommentPresenterIsEmpty [
	self assert: criticBrowser noteBookPageRuleCommentPresenter text equals: ''
]

{ #category : #initialize }
CBCriticBrowserTest >> testRulesPresenterAnySelectedItem [
	self assert: criticBrowser rulesPresenter  selectedItem equals: nil.
]

{ #category : #'action-RulesPresenter' }
CBCriticBrowserTest >> testRulesPresenterClickaCompositeNodeShouldRuleCommentPresenterShouldEmpty [
	criticBrowser rulesPresenter selectPath: #(1).
	self assert: criticBrowser selectedRule isComposite.
	self assert: criticBrowser noteBookPageRuleCommentPresenter text equals: ''
]

{ #category : #'action-RulesPresenter' }
CBCriticBrowserTest >> testRulesPresenterClickaCompositeNodeShouldTransmitNothingToCriticsPresenter [
	criticBrowser rulesPresenter selectPath: #(1).
	self assert: criticBrowser selectedRule isComposite.
	self assert: criticBrowser criticsPresenter items isEmpty
]

{ #category : #'action-RulesPresenter' }
CBCriticBrowserTest >> testRulesPresenterClickaNotCompositeShouldTransmitCriticsToCriticsPresenter [
	"hasSameElements: (criticBrowserPresenter criticsOf: criticBrowserPresenter rule)"

	self selectInstanceVariableCapitalizationRule.
	self deny: criticBrowser selectedRule isComposite.
	self assert: criticBrowser criticsPresenter items size equals: 3
]

{ #category : #'action-RulesPresenter' }
CBCriticBrowserTest >> testRulesPresenterClickaNotCompositeShouldTransmitDescriptionToSourceCodePresenter [
	self selectInstanceVariableCapitalizationRule.
	self deny: criticBrowser selectedRule isComposite.
	self assert: criticBrowser noteBookPageRuleCommentPresenter text equals: 'Instance variable names on the instance- and class-side should start with a lowercase letter.'
]

{ #category : #initialize }
CBCriticBrowserTest >> testSelectedRuleIsNilShouldRuleCommentPresenterIsEmpty [
	self assert: criticBrowser noteBookPageRuleCommentPresenter text equals: ''.
]

{ #category : #'action-SourceCodePresenter' }
CBCriticBrowserTest >> testSourceCodePresenterChangeTextAndAcceptShouldSameAsCompiledMethod [
	| beforeModif |
	"n'enregistre pas les modification"
	self selectUnclassifiedMethodRule.
	beforeModif := criticBrowser noteBookPageCriticErrorPresenter text.
	criticBrowser noteBookPageCriticErrorPresenter text: beforeModif , '"klm"'.
	criticBrowser noteBookPageCriticErrorPresenter accept.
	criticBrowser criticsPresenter clickOnSelectedItem.
	criticBrowser criticsPresenter clickOnSelectedItem.
	self
		assert: criticBrowser noteBookPageCriticErrorPresenter text
		equals: beforeModif , '"klm"'.
	criticBrowser noteBookPageCriticErrorPresenter text: beforeModif.
	criticBrowser noteBookPageCriticErrorPresenter accept
]

{ #category : #tests }
CBCriticBrowserTest >> testWhenAddInstanceVariableCapitalisedShouldAddItToTheToSortColumn [
	self timeLimit: 1 minute.
	[ | nbOfToSort rule |
	self selectInstanceVariableCapitalizationRule.
	rule := ((criticBrowser rulesPresenter roots at: 1) rules
		select: [ :aRule | aRule isKindOf: GRInstanceVariableCapitalizationRule ])
		at: 1.	"ugly sorry"
	nbOfToSort := criticBrowser computeToSort: rule.
	UseForTestCriticBrowser addInstVarNamed: 'O'.
	self
		assert: (criticBrowser computeToSort: rule)
		equals: nbOfToSort + 1 ]
		ensure: [ UseForTestCriticBrowser removeInstVarNamed: 'O' ]
]
