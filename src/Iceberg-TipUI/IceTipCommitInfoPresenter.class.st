"
Description
--------------------

I'm a panel to show the history of a commitish (usually a branch or a tag). I display infos on it and diffs.

I'm usually used as part of an IceTipHistoryBrowser.
"
Class {
	#name : #IceTipCommitInfoPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'model',
		'selectedModel',
		'commitTabPanel',
		'infoTab',
		'emptyTab',
		'headTabContainer',
		'ancestorTabContainer',
		'headTab',
		'ancestorTab'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipCommitInfoPresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		add: #commitTabPanel;
		yourself
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> beForCommit [
	headTabContainer := IceTipHistoryDiffFromHead tab: headTab.
	ancestorTabContainer := IceTipHistoryDiffToAncestor tab: ancestorTab
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> beForMerge [
	headTabContainer := IceTipHistoryDiffToHead tab: headTab.
	ancestorTabContainer := IceTipHistoryDiffToAncestor tab: ancestorTab
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> commitTabPanel [
	^ commitTabPanel
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> commitTabPanel: anObject [
	commitTabPanel := anObject
]

{ #category : #initialization }
IceTipCommitInfoPresenter >> initialize [
	self beForCommit.
	super initialize
]

{ #category : #initialization }
IceTipCommitInfoPresenter >> initializeCommitPanel [
	"This will be initialised in fact on the first refresh"

	commitTabPanel
		addTab: (infoTab := self newInfoTab);
		addTab: (headTab := self newDiffTab 
			label: 'Diff from HEAD';
			whenTabSelected: [ self refreshDiffFromHead ];
			yourself);
		addTab: (ancestorTab := self newDiffTab 
			label: 'Diff from ancestor';
			whenTabSelected: [ self refreshDiffFromAncestor ];
			yourself).
		
	headTabContainer tab: headTab.
	ancestorTabContainer tab: ancestorTab
]

{ #category : #initialization }
IceTipCommitInfoPresenter >> initializeWidgets [
	commitTabPanel := self newTabManager.
	
	self initializeCommitPanel.
	
	self focusOrder add: commitTabPanel
]

{ #category : #'private factory' }
IceTipCommitInfoPresenter >> newDiffTab [
	^ self newTab 
		label: 'Diff';
		icon: (self iconNamed: #diff);
		closeable: false;
		yourself
]

{ #category : #'private factory' }
IceTipCommitInfoPresenter >> newEmptyTab [
	^ self newTab 
		label: 'Info';
		icon: (self iconNamed: #smallQuestion);
		closeable: false;
		contents: '(No commit selected)' asMorph;
		yourself
]

{ #category : #'private factory' }
IceTipCommitInfoPresenter >> newInfoTab [
	^ self newTab 
		label: 'Info';
		icon: (self iconNamed: #smallQuestion);
		closeable: false;
		yourself
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> refresh [
	selectedModel ifNotNil: [ self showInfoTabs ] ifNil: [ self showEmptyTab ]
]

{ #category : #private }
IceTipCommitInfoPresenter >> refreshCommitInfo [
	infoTab contents: (selectedModel info ifNotNil: [ (IceTipReadOnlyForm items: selectedModel info) buildWithSpec ])
]

{ #category : #private }
IceTipCommitInfoPresenter >> refreshDiffFromAncestor [
	ancestorTabContainer updateContentsOn: selectedModel
]

{ #category : #private }
IceTipCommitInfoPresenter >> refreshDiffFromAncestorLabel [
	ancestorTabContainer updateLabelOn: selectedModel
]

{ #category : #private }
IceTipCommitInfoPresenter >> refreshDiffFromHead [
	headTabContainer updateContentsOn: selectedModel
]

{ #category : #private }
IceTipCommitInfoPresenter >> refreshDiffFromHeadLabel [
	headTabContainer updateLabelOn: selectedModel
]

{ #category : #private }
IceTipCommitInfoPresenter >> removeAllCommitPanelTabs [
	commitTabPanel tabs copy do: [ :each | commitTabPanel removeTab: each ]
]

{ #category : #accessing }
IceTipCommitInfoPresenter >> selectedModel: aModel [
	selectedModel := aModel.
	self refresh
]

{ #category : #'model access' }
IceTipCommitInfoPresenter >> setModelBeforeInitialization: anObject [
	model := anObject
]

{ #category : #private }
IceTipCommitInfoPresenter >> showEmptyTab [
	emptyTab ifNil: [ emptyTab := self newEmptyTab ].
	self removeAllCommitPanelTabs.
	commitTabPanel addTab: emptyTab.
]

{ #category : #private }
IceTipCommitInfoPresenter >> showInfoTabs [
	"This is just an optimisation to not refresh always. This way it will happen 
	 just if we come from a 'non-selection'"
	(commitTabPanel tabs includes: infoTab) ifFalse: [
		self removeAllCommitPanelTabs.
		commitTabPanel 
			addTab: infoTab;
			addTab: headTabContainer tab;
			addTab: ancestorTabContainer tab ]. 
	
	self refreshCommitInfo.
	"refresh tab headers"
	self refreshDiffFromHeadLabel.
	self refreshDiffFromAncestorLabel.
	"Refresh selected tab contents"
	commitTabPanel selectedTab ifNotNil: #tabSelected.

	
]
