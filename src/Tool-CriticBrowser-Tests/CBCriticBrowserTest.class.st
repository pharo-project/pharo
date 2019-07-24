Class {
	#name : #CBCriticBrowserTest,
	#superclass : #CBAbstractTest,
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #tests }
CBCriticBrowserTest >> testAddingInstanceVariableCapitalisedShouldAddItToTheToSortColumn [
	[ | nbOfToSort rule |
	self selectInstanceVariableCapitalizationRule.
	rule := self styleRules detect: [ :aRule | aRule class uniqueIdentifierName = #GRInstanceVariableCapitalizationRule ].
	nbOfToSort := criticBrowser computeToSort: rule.
	UseForTestCriticBrowser addInstVarNamed: 'O'.
	self assert: (criticBrowser computeToSort: rule) equals: nbOfToSort + 1 ]
		ensure: [ UseForTestCriticBrowser removeInstVarNamed: 'O' ]
]

{ #category : #tests }
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

{ #category : #'tests - initialization' }
CBCriticBrowserTest >> testInitCriticErrorPresenterIsEmpty [
	self assertEmpty: criticBrowser noteBookPageCriticErrorPresenter text
]

{ #category : #'tests - initialization' }
CBCriticBrowserTest >> testInitRuleCommentPresenterIsEmpty [
	self assertEmpty: criticBrowser noteBookPageRuleCommentPresenter text
]

{ #category : #'tests - initialization' }
CBCriticBrowserTest >> testNoRuleSelectedByDefault [
	self assert: criticBrowser selectedRule isNil.
	self assertEmpty: criticBrowser criticsPresenter items
]

{ #category : #tests }
CBCriticBrowserTest >> testRulesPresenterClickaCompositeNodeShouldRuleCommentPresenterShouldEmpty [
	criticBrowser rulesPresenter selectPath: #(1).
	self assert: criticBrowser selectedRule isComposite.
	self assertEmpty: criticBrowser noteBookPageRuleCommentPresenter text
]

{ #category : #tests }
CBCriticBrowserTest >> testRulesPresenterClickaCompositeNodeShouldTransmitNothingToCriticsPresenter [
	criticBrowser rulesPresenter selectPath: #(1).
	self assert: criticBrowser selectedRule isComposite.
	self assertEmpty: criticBrowser criticsPresenter items
]

{ #category : #tests }
CBCriticBrowserTest >> testRulesPresenterClickaNotCompositeShouldTransmitCriticsToCriticsPresenter [
	self selectInstanceVariableCapitalizationRule.
	self deny: criticBrowser selectedRule isComposite.
	self assert: criticBrowser criticsPresenter items size equals: 3
]

{ #category : #tests }
CBCriticBrowserTest >> testRulesPresenterClickaNotCompositeShouldTransmitDescriptionToSourceCodePresenter [
	self selectInstanceVariableCapitalizationRule.
	self deny: criticBrowser selectedRule isComposite.
	self assert: criticBrowser noteBookPageRuleCommentPresenter text equals: 'Instance variable names on the instance- and class-side should start with a lowercase letter.'
]

{ #category : #tests }
CBCriticBrowserTest >> testSelectedRuleIsNilShouldRuleCommentPresenterIsEmpty [
	self assertEmpty: criticBrowser noteBookPageRuleCommentPresenter text
]

{ #category : #tests }
CBCriticBrowserTest >> testSourceCodePresenterChangeTextAndAcceptShouldSameAsCompiledMethod [
	| beforeModif |
	self selectUnclassifiedMethodRule.
	beforeModif := criticBrowser noteBookPageCriticErrorPresenter text.
	criticBrowser noteBookPageCriticErrorPresenter text: beforeModif , '"klm"'.
	criticBrowser noteBookPageCriticErrorPresenter accept.
	criticBrowser criticsPresenter clickOnSelectedItem.
	criticBrowser criticsPresenter clickOnSelectedItem.
	self assert: criticBrowser noteBookPageCriticErrorPresenter text equals: beforeModif , '"klm"'.
	criticBrowser noteBookPageCriticErrorPresenter text: beforeModif.
	criticBrowser noteBookPageCriticErrorPresenter accept
]
