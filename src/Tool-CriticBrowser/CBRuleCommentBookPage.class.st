Class {
	#name : #CBRuleCommentBookPage,
	#superclass : #CBAbstractNoteBookPage,
	#category : #'Tool-CriticBrowser-utilities-tabContent'
}

{ #category : #accessing }
CBRuleCommentBookPage class >> layoutSpec [
	^ #ruleCommentBookPage
]

{ #category : #accessing }
CBRuleCommentBookPage class >> provider: aCriticBrowser [
	aCriticBrowser ruleCommentBookPage: (aCriticBrowser instantiate: TextPresenter new)
]

{ #category : #accessing }
CBRuleCommentBookPage class >> title [
	^ 'comment Rule'
]
