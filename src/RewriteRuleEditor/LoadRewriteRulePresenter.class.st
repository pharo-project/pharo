"
A simple GUI that permits to load a custom rule into RewriteBasicEditorPresenter.
"
Class {
	#name : #LoadRewriteRulePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'rulesList',
		'buttonBar'
	],
	#category : #'RewriteRuleEditor-BasicEditor'
}

{ #category : #specs }
LoadRewriteRulePresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #rulesList;
		addLast: #buttonBar;
		yourself
]

{ #category : #accessing }
LoadRewriteRulePresenter class >> icon [ 
	^ self iconNamed: #smallFind
]

{ #category : #api }
LoadRewriteRulePresenter class >> open [
	<script>
	^ self new openWithSpec
]

{ #category : #actions }
LoadRewriteRulePresenter >> close [
	self window close
]

{ #category : #initialization }
LoadRewriteRulePresenter >> initializePresenters [
	rulesList := self newList
		items:
			(RBCustomTransformationRule subclasses collect: [ :each | each ])
				asOrderedCollection;
		yourself.
	buttonBar := self newActionBar
		add:
			(self newButton
				label: 'Load';
				icon: (self iconNamed: #smallDoIt);
				action: [ self loadRule. self close ]);
		add:
			(self newButton
				label: 'Close';
				icon: (self iconNamed: #smallCancel);
				action: [ self close ]);
		yourself
]

{ #category : #initialization }
LoadRewriteRulePresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Rewrite rule loader';
		initialExtent: 300 @ 300;
		windowIcon: (self class icon);
		askOkToClose: false;
		aboutText: 'Opens BasicTransformationPresenter with a custom rule'
]

{ #category : #actions }
LoadRewriteRulePresenter >> loadRule [
	| selectedRule |
	rulesList selectedItems
		ifEmpty: [ self noRuleSelectedAlert.
			^ self ].
	selectedRule := rulesList selectedItems first new.
	^ RewriteBasicEditorPresenter new
		searchForCode: selectedRule searchFor;
		replaceWithCode: selectedRule replaceWith;
		openWithSpec
]

{ #category : #defaults }
LoadRewriteRulePresenter >> noRuleSelectedAlert [
	UIManager default alert: 'You have to select a rule.'
]

{ #category : #accessing }
LoadRewriteRulePresenter >> rulesList [
	^ rulesList
]
