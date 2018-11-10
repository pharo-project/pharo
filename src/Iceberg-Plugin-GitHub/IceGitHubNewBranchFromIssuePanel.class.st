"
I'm a panel to create a new branch taking a GitHub issue as basis.
A GitHub issue will be created in the form: 

123-a-github-issue
"
Class {
	#name : #IceGitHubNewBranchFromIssuePanel,
	#superclass : #IceTipCheckoutNewBranchPanel,
	#instVars : [
		'remotePanel',
		'issueNumberText',
		'issueLabel',
		'issueText',
		'issue',
		'fetched',
		'selectRemoteModel',
		'currentWaitingProcess'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceGitHubNewBranchFromIssuePanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed
		newColumn: [ :col | 
			col
				newRow: #currentBranchLabel height: self inputTextHeight;
				newRow: #remotePanel height: self inputTextHeight;
				newRow: [ :row | 
					row
						newColumn: #branchLabel width: self iceLabelWidth;
						newColumn: #issueNumberText ]
					height: self inputTextHeight;
				newRow: [ :row |
					row 
						newColumn: #issueLabel width: self iceLabelWidth;
						newColumn: #issueText ] 
					height: self inputTextHeight;
				newRow: #spacePanel ]
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel class >> icon [

	^ (self iconNamed: #github) scaledToSize: 16@16
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel class >> title [
	^ 'New branch from issue'
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel class >> unknownTitle [
	^ 'INVALID-ISSUE'
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel >> branchName [
	
	^ '{1}-{2}' format: { 
		issueNumberText text trim. 
		issueText text trim }.
]

{ #category : #api }
IceGitHubNewBranchFromIssuePanel >> extent [
	^ 800@175
]

{ #category : #initialization }
IceGitHubNewBranchFromIssuePanel >> initialize [

	super initialize.
	fetched := false
]

{ #category : #initialization }
IceGitHubNewBranchFromIssuePanel >> initializeFocusOrder [

	self focusOrder
		add: remotePanel;
		add: issueNumberText;
		add: issueText
]

{ #category : #initialization }
IceGitHubNewBranchFromIssuePanel >> initializeWidgetsContents [
	spacePanel := PanelMorph new asSpecAdapter.
	
	self initializeCurrentBranchLabel.
	
	remotePanel := self instantiate: IceTipSelectRemotePresenter on: self selectRemoteModel.
	remotePanel remoteList whenSelectedItemChanged: [ :remoteModel | 
		self useRemote: remoteModel  ].

	branchLabel := self newLabel label: 'Issue number:'.
	issueNumberText := self newTextInput
		ghostText: 'e.g., 123';
		autoAccept: true.
	issueLabel := self newLabel label: 'Title:'.
	issueText := self newTextInput
		ghostText: 'e.g., 123-github-issue';
		autoAccept: true.
		
	issueNumberText whenBuiltDo: [ :w | w widget wrapFlag: false ].
	issueText
		whenBuiltDo: [ :w | 
			w widget wrapFlag: false.
			w widget enabled: false ].
	issueNumberText textHolder whenChangedDo: [ :text | self validateIssue: text ]
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueLabel [
	^ issueLabel
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueLabel: anObject [
	issueLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueNumberText [
	^ issueNumberText
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueNumberText: anObject [
	issueNumberText := anObject
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueText [
	^ issueText
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> issueText: anObject [
	issueText := anObject
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> remotePanel [
	^ remotePanel
]

{ #category : #'accessing ui' }
IceGitHubNewBranchFromIssuePanel >> remotePanel: anObject [
	remotePanel := anObject
]

{ #category : #private }
IceGitHubNewBranchFromIssuePanel >> sanitizeTitle: aString [
	| separators safeSeparator |
	
	aString isEmptyOrNil ifTrue: [ ^ self class unknownTitle ].
	
	safeSeparator := $-.
	separators := { Character space. Character tab. $_. safeSeparator }.
	^ String streamContents: [ :str | 
		aString do: [ :c |
			(separators includes: c) ifTrue: [ str nextPut: safeSeparator ].
			c isAlphaNumeric ifTrue: [ str nextPut: c ] ] ]
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel >> selectRemoteModel [

	^ selectRemoteModel ifNil: [
		selectRemoteModel := IceTipRemoteActionModel 
			repositoryModel: self model
			on: self model entity ]
]

{ #category : #private }
IceGitHubNewBranchFromIssuePanel >> updateText: aString [ 

	issueText text: aString.
	fetched := true
]

{ #category : #private }
IceGitHubNewBranchFromIssuePanel >> useRemote: aRemoteModel [

	self selectRemoteModel useRemote: aRemoteModel
]

{ #category : #accessing }
IceGitHubNewBranchFromIssuePanel >> validate [
	
	self
		assert: ((issueNumberText text asString notEmpty) and: [ issueNumberText text isAllDigits ])
		description: 'Please enter a valid issue number'.
		
	self
		assert: fetched
		description: 'Please fetch a valid issue number'
]

{ #category : #private }
IceGitHubNewBranchFromIssuePanel >> validateIssue: aString [
	| number remote |

	fetched := false.
	aString trimmed isEmpty ifTrue: [ ^ self ].
	aString isAllDigits ifFalse: [ ^ self ].
	number := aString asNumber.
	(issue isNil or: [ (issue at: 'number') ~= number ]) ifFalse: [ ^ self ].
	remote := self selectRemoteModel remote.
	
	self wait: 500 milliSeconds thenDo: [ 	
		[
			issue := IceGitHubAPI new 
				beAnonymous;
				getIssue: remote owner 
					project: remote projectName 
					number: number.
			UIManager default defer: [ 
				self updateText: (self sanitizeTitle: (issue at: 'title')) ] ]
		on: IceGitHubError do: [ :e | 
			e messageText = 'Not Found' ifFalse: [ e pass ].
			 UIManager default defer: [ 
				self updateText: self class unknownTitle ] ] ]
]

{ #category : #private }
IceGitHubNewBranchFromIssuePanel >> wait: time thenDo: aBlock [
	"This method avoids to request too many times the github api. 
	 It does it in the simplest way possible: it starts a process that waits some 
	 time (usually 500 milliseconds). If there is another entry before, it terminates 
	 the process and starts a new one."

	currentWaitingProcess ifNotNil: [ currentWaitingProcess terminate ].
	currentWaitingProcess := [ 
			time wait.
			aBlock value ] 
		forkNamed: 'GitHub issue tracker'
]
