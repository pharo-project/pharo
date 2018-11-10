"
I'm a browser to commit changes to a repository.

(self onRepository: IceRepository registry anyOne) openWithSpec
"
Class {
	#name : #IceTipCommitBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'diffPanel',
		'commentPanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipCommitBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: [ :row |
					row newColumn: [ :column |
						column 
							newRow: #diffPanel top: 0.35;
							addHSplitter;
							newRow: #commentPanel bottom: 0.65 ] ] ]
]

{ #category : #'instance creation' }
IceTipCommitBrowser class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository)
]

{ #category : #actions }
IceTipCommitBrowser >> accept [
	^ self doCommit
]

{ #category : #'accessing ui' }
IceTipCommitBrowser >> commentPanel [
	^ commentPanel
]

{ #category : #'accessing ui' }
IceTipCommitBrowser >> commentPanel: anObject [
	commentPanel := anObject
]

{ #category : #accessing }
IceTipCommitBrowser >> contextClass [
	^ IceTipCommitContext
]

{ #category : #'accessing ui' }
IceTipCommitBrowser >> diffPanel [
	^ diffPanel
]

{ #category : #'accessing ui' }
IceTipCommitBrowser >> diffPanel: anObject [
	diffPanel := anObject
]

{ #category : #actions }
IceTipCommitBrowser >> doCommit [
	self 
		doCommit: diffPanel selectedItems 
		message: commentPanel message
		pushing: commentPanel isPushing
]

{ #category : #actions }
IceTipCommitBrowser >> doCommit: aCollection message: aString pushing: aBoolean [
	self model
		commit:
			(IceTipCommitAction new
				diff: self model workingCopyDiff;
				items: aCollection;
				message: aString;
				yourself)
		then: [ self verifyNeedsRefreshOrClose.
			aBoolean
				ifTrue: [ (IceTipPushAction new repository: self model entity) execute ] ]
]

{ #category : #accessing }
IceTipCommitBrowser >> initialExtent [
	^ (800@600) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipCommitBrowser >> initializeWidgets [
	super initializeWidgets.
	
	diffPanel := self instantiate: IceTipDiffSelectingPanel on: self model workingCopyDiff.
	diffPanel leftLabel: 'Repository'.
	diffPanel rightLabel: 'Working Copy'.
	
	commentPanel := self instantiate: IceTipCommentPanel on: self model.
	commentPanel onCommit: [ self doCommit ].
	"a hack because text comment cannot react to cmd+s 'as usual'."
	commentPanel commentText whenBuiltDo: [ :ann |  
		self addShortcutsTo: ann widget textArea ].
			
	self whenBuiltDo: [ :ann |  self addShortcutsTo: ann widget ].

	self focusOrder 
		add: commentPanel;
		add: diffPanel
]

{ #category : #accessing }
IceTipCommitBrowser >> refresh [
	diffPanel refresh
]

{ #category : #private }
IceTipCommitBrowser >> refreshWhenRepository: ann [
	(self model isModelOf: ann repository)
		ifFalse: [ ^ self ].
	self model reset.
	self refresh
]

{ #category : #initialization }
IceTipCommitBrowser >> subscribeToAnnouncements [
	self announcer 
		when: IceTipDiffRefreshed 
		send: #refreshWhenRepository: 
		to: self
]

{ #category : #accessing }
IceTipCommitBrowser >> title [
	^ 'Commit on {1} branch {2}' format: { 
		self model name. 
		self model branchName } 
]

{ #category : #private }
IceTipCommitBrowser >> verifyNeedsRefreshOrClose [
	"Verifies if there are remaining changes to commit. 
	 In case there are none, close the window."
	
	self model reset.
	diffPanel model reset.
	diffPanel model isEmpty
		ifTrue: [ ^ self window close ].
	self commentPanel clear. 
	diffPanel basicRefresh
]
