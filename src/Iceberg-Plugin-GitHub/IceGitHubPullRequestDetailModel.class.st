"
I'm a spec model to present a detailed view of a Pull Request.
"
Class {
	#name : #IceGitHubPullRequestDetailModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'urlLabel',
		'urlText',
		'authorLabel',
		'authorText',
		'fromLabel',
		'fromText',
		'toLabel',
		'toText',
		'mergeableLabel',
		'mergeablePanel',
		'statusLabel',
		'statusPanel',
		'checksTable',
		'bodyText',
		'pullRequest'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceGitHubPullRequestDetailModel class >> defaultSpec [ 
	<spec>

	^ SpecLayout composed
		newColumn: [ :c |
			c 
				newRow: [:row | row add: #urlLabel width: 80 * World displayScaleFactor; add: #urlText ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #authorLabel width: 80 * World displayScaleFactor; add: #authorText ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #fromLabel width: 80 * World displayScaleFactor; add: #fromText ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #toLabel width: 80 * World displayScaleFactor; add: #toText ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #mergeableLabel width: 80 * World displayScaleFactor; add: #mergeablePanel ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #statusLabel width: 80 * World displayScaleFactor; add: #statusPanel ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #checksTable ];
				newRow: [:row | row add: #bodyText ] ];
		yourself
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> authorLabel [
	^ authorLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> authorLabel: anObject [
	authorLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> authorText [
	^ authorText
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> authorText: anObject [
	authorText := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> bodyText [
	^ bodyText
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> bodyText: anObject [
	bodyText := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> checksTable [
	^ checksTable
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> checksTable: anObject [
	checksTable := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> fromLabel [
	^ fromLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> fromLabel: anObject [
	fromLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> fromText [
	^ fromText
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> fromText: anObject [
	fromText := anObject
]

{ #category : #private }
IceGitHubPullRequestDetailModel >> iconForMergeable: mergeable [
	mergeable = true ifTrue: [ ^ self iconNamed: #smallOk ].
	^ self iconNamed: #smallCancel
]

{ #category : #private }
IceGitHubPullRequestDetailModel >> iconForStatus: status [
	status = 'success' ifTrue: [ ^ self iconNamed: #testGreen ].
	status = 'failure' ifTrue: [ ^ self iconNamed: #testRed ].
	status = 'pending' ifTrue: [ ^ self iconNamed: #testYellow ].
	
	^ self iconNamed: #testNotRun
]

{ #category : #initialization }
IceGitHubPullRequestDetailModel >> initializeWidgets [
	urlLabel := self newLabel label: 'Url'.
	authorLabel := self newLabel label: 'Author'.
	fromLabel := self newLabel label: 'From'.
	toLabel := self newLabel label: 'Into'.
	mergeableLabel := self newLabel label: 'Mergeable'.
	statusLabel := self newLabel label: 'Status'.

	urlText := self newTextInput enabled: false.
	authorText  := self newTextInput enabled: false.
	fromText  := self newTextInput enabled: false.
	toText  := self newTextInput enabled: false.
	mergeablePanel := self newMergeablePanel.
	statusPanel := self newStatusPanel.
	checksTable := self newChecksTable.
	bodyText := self newText enabled: false.
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> mergeableLabel [
	^ mergeableLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> mergeableLabel: anObject [
	mergeableLabel := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDetailModel >> mergeablePanel [
	^ mergeablePanel
]

{ #category : #accessing }
IceGitHubPullRequestDetailModel >> mergeablePanel: anObject [
	mergeablePanel := anObject
]

{ #category : #'private factory' }
IceGitHubPullRequestDetailModel >> newChecksTable [
	^ FTTableMorph new 
		addColumn: ((FTColumn id: #target_url) width: 18 * World displayScaleFactor);
		addColumn: ((FTColumn id: #state) width: 65 * World displayScaleFactor);
		addColumn: ((FTColumn id: #context) width: 100 * World displayScaleFactor);
		addColumn: (FTColumn id: #description);
		cellInset: 5 * World displayScaleFactor;
		beResizable;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		asSpecAdapter
		
]

{ #category : #'private factory' }
IceGitHubPullRequestDetailModel >> newMergeablePanel [
	^ PanelMorph new 
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellInset: 5;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #'private factory' }
IceGitHubPullRequestDetailModel >> newStatusPanel [
	^ PanelMorph new 
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellInset: 5;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #accessing }
IceGitHubPullRequestDetailModel >> pullRequest [
	^ pullRequest
]

{ #category : #initialization }
IceGitHubPullRequestDetailModel >> pullRequest: aPullRequest [
	pullRequest := aPullRequest.
	urlText text: self pullRequest url.
	authorText text: self pullRequest author.
	fromText text: self pullRequest headLabel.
	toText text: self pullRequest baseLabel.
	bodyText text: self pullRequest body.
	self retrieveStatuses.
	self retrieveDetail
]

{ #category : #private }
IceGitHubPullRequestDetailModel >> retrieveDetail [
	
	self mergeablePanel widget addMorphBack: 'Loading...' asMorph.
	[ | detail mergeable |  
		[  
			detail := self pullRequest detail.
			mergeable := detail at: #mergeable ]
		on: Error do: [ :e | mergeable := nil ].
		UIManager default defer: [ 
			self mergeablePanel widget
				removeAllMorphs;
				addMorphBack: (self iconForMergeable: mergeable) asMorph;
				addMorphBack: (self textForMergeable: mergeable) asMorph ]
	] fork
]

{ #category : #private }
IceGitHubPullRequestDetailModel >> retrieveStatuses [
	self statusPanel widget addMorphBack: 'Loading...' asMorph.
	[ 
	| status generalStatus statuses |
	
	[
		status := self pullRequest status.
		generalStatus := status at: 'state'.
		statuses := status at: 'statuses' ]
	on: Error do: [ :e | 
		generalStatus := 'Unknown'.
		statuses := #() ].
	
	UIManager default defer: [ 
		self statusPanel widget
			removeAllMorphs;
			addMorphBack: (self iconForStatus: generalStatus) asMorph;
			addMorphBack: generalStatus asMorph.
	 	self checksTable widget dataSource: (IceGitHubPullRequestDataSource elements: statuses) ]
	] fork
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> statusLabel [
	^ statusLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> statusLabel: anObject [
	statusLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> statusPanel [
	^ statusPanel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> statusPanel: anObject [
	statusPanel := anObject
]

{ #category : #private }
IceGitHubPullRequestDetailModel >> textForMergeable: mergeable [
	mergeable = true ifTrue: [ ^ 'Yes' ].
	mergeable = false ifTrue: [ ^ 'There are conflicts' ].
	^ mergeable asString
	
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> toLabel [
	^ toLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> toLabel: anObject [
	toLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> toText [
	^ toText
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> toText: anObject [
	toText := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> urlLabel [
	^ urlLabel
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> urlLabel: anObject [
	urlLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> urlText [
	^ urlText
]

{ #category : #'accessing ui' }
IceGitHubPullRequestDetailModel >> urlText: anObject [
	urlText := anObject
]
