"
I'm a dialog to ask for issue number to create correct branchs for Pharo Development.
"
Class {
	#name : #IcePharoNewBranchFromIssuePanel,
	#superclass : #IceTipCheckoutNewBranchPanel,
	#instVars : [
		'issueNumberText',
		'issueLabel',
		'issueText',
		'issue',
		'fetched'
	],
	#category : #'Iceberg-Plugin-Pharo-Manuscript'
}

{ #category : #specs }
IcePharoNewBranchFromIssuePanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed
		newColumn: [ :col | 
			col
				newRow: #currentBranchLabel height: self inputTextHeight;
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
IcePharoNewBranchFromIssuePanel class >> icon [

	^ (Smalltalk ui iconNamed: #pharo) scaledToSize: 16@16
]

{ #category : #accessing }
IcePharoNewBranchFromIssuePanel class >> title [
	^ 'New branch from issue'
]

{ #category : #accessing }
IcePharoNewBranchFromIssuePanel class >> unknownTitle [
	^ 'INVALID-ISSUE'
]

{ #category : #accessing }
IcePharoNewBranchFromIssuePanel >> branchName [
	
	^ '{1}-{2}' format: { 
		issueNumberText text trim. 
		issueText text trim }.
]

{ #category : #api }
IcePharoNewBranchFromIssuePanel >> extent [
	^ 800@175
]

{ #category : #initialization }
IcePharoNewBranchFromIssuePanel >> initialize [

	super initialize.
	fetched := false
]

{ #category : #initialization }
IcePharoNewBranchFromIssuePanel >> initializeFocusOrder [
	self focusOrder
		add: issueNumberText;
		add: issueText
]

{ #category : #initialization }
IcePharoNewBranchFromIssuePanel >> initializeWidgetsContents [
	spacePanel := PanelMorph new asSpecAdapter.
	self initializeCurrentBranchLabel.
	branchLabel := self newLabel label: 'Pharo issue Number'.
	issueNumberText := self newTextInput
		ghostText: 'e.g., 12345';
		autoAccept: true.
	issueLabel := self newLabel label: 'Title'.
	issueText := self newTextInput
		ghostText: 'e.g., a-fogbugz-issue';
		autoAccept: true.
		
	issueNumberText whenBuiltDo: [ :w | w widget wrapFlag: false ].
	issueText
		whenBuiltDo: [ :w | 
			w widget wrapFlag: false.
			w widget enabled: false ].
	issueNumberText textHolder whenChangedDo: [ :text | self validateIssue: text ]
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueLabel [
	^ issueLabel
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueLabel: anObject [
	issueLabel := anObject
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueNumberText [
	^ issueNumberText
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueNumberText: anObject [
	issueNumberText := anObject
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueText [
	^ issueText
]

{ #category : #'accessing ui' }
IcePharoNewBranchFromIssuePanel >> issueText: anObject [
	issueText := anObject
]

{ #category : #private }
IcePharoNewBranchFromIssuePanel >> sanitizeTitle: aString [
	| separators safeSeparator |
	
	aString isEmptyOrNil ifTrue: [ ^ self class unknownTitle ].
	
	safeSeparator := $-.
	separators := { Character space. Character tab. $_. safeSeparator }.
	^ String streamContents: [ :str | 
		aString do: [ :c |
			(separators includes: c) ifTrue: [ str nextPut: safeSeparator ].
			c isAlphaNumeric ifTrue: [ str nextPut: c ] ] ]
]

{ #category : #private }
IcePharoNewBranchFromIssuePanel >> updateText: aString [ 
	issueText text: aString.
	fetched := true
]

{ #category : #accessing }
IcePharoNewBranchFromIssuePanel >> validate [
	
	self
		assert: ((issueNumberText text size >= 5) and: [ issueNumberText text isAllDigits ])
		description: 'Please enter a valid issue number'.
		
	self
		assert: fetched
		description: 'Please fetch a valid issue number'
]

{ #category : #private }
IcePharoNewBranchFromIssuePanel >> validateIssue: aString [
	| number |

	fetched := false.
	aString size = 5 ifFalse: [ ^ self ].
	aString isAllDigits ifFalse: [ ^ self ].
	number := aString asNumber.
	(issue isNil or: [ issue number ~= number ]) ifFalse: [ ^ self ].
	
	issue := PharoIssue number: number.
	[ 	"Fork to allow UI to continue"
		issue downloadTitle.
		UIManager default defer: [ 
			self updateText: (self sanitizeTitle: issue title) ] ] fork
]
