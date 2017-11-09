"
MCSliceMaker is a dialog to help you making slices.
It allows you to enter the slice number, the summary and to directly select dependent dirty packages.
Then a slice package is added for you in the working copy browser from which it is opened.
Just copy-paste issue number and summary. All is formated for you.

Instance Variables
	info:		<MCSliceInfo>
	okToDoSlice:		<Boolean>
	window:		<StandardWindow>

info
	- It is the model for the user interface

okToDoSlice
	- true if the OK button is clicked, it is to avoid slice making in case of cancel and in the case where the close button of the window has been clicked

window
	- my window

"
Class {
	#name : #MCSliceMaker,
	#superclass : #Model,
	#instVars : [
		'info',
		'window',
		'okToDoSlice'
	],
	#category : #MonticelloGUI
}

{ #category : #opening }
MCSliceMaker class >> openFor: anotherWindow [
	^ self new openFor: anotherWindow
]

{ #category : #'user interface' }
MCSliceMaker >> cancel [
	self cleanUp.
	self noSlice
]

{ #category : #private }
MCSliceMaker >> cleanUp [
	info removeDependent: self.
	window delete

]

{ #category : #actions }
MCSliceMaker >> downloadIssueTitle [
	[self info setTitleFromIssue]
		on: Error
		do: [:ex | ^ self informFailedWith: ex].
]

{ #category : #accessing }
MCSliceMaker >> info [
	^ info 
]

{ #category : #actions }
MCSliceMaker >> informFailedWith: anError [
	self inform: anError asString.
	self info title: '------'
]

{ #category : #'user interface' }
MCSliceMaker >> initialExtent [
	^ 800@400
]

{ #category : #initialization }
MCSliceMaker >> initialize [
	super initialize.
	info := MCSliceInfo new.
	info addDependent: self.
	

]

{ #category : #'user interface' }
MCSliceMaker >> issueNumberEntryOn: canvas [
	| issueNumberEntry |
	issueNumberEntry := canvas
				newTextEntryFor: self info
				get: #issueNumber
				set: #issueNumber:
				class: Integer
				getEnabled: nil
				help: 'Fogbugz issue number' translated.
	issueNumberEntry
		hResizing: #rigid;
		acceptOnFocusChange: true;
		width: (StandardFonts defaultFont widthOfString: 'Issue number0');
		ghostText: 'Issue number'.
	^ issueNumberEntry
]

{ #category : #'user interface' }
MCSliceMaker >> issueTitleOn: canvas [
	| sliceTitleEntry |
	sliceTitleEntry := canvas window
				newAutoAcceptTextEntryFor: self info
				get: #title
				set: #title:
				class: String
				getEnabled: nil
				help: 'An identification label for the slice. Just copy-paste the Fogbugz issue title here' translated.
	sliceTitleEntry ghostText: 'Just paste the issue title here'.
	^ sliceTitleEntry
]

{ #category : #'user interface' }
MCSliceMaker >> noSlice [
	info := nil.

]

{ #category : #'user interface' }
MCSliceMaker >> ok [
	self okToDoSlice: true.
	self cleanUp
]

{ #category : #'user interface' }
MCSliceMaker >> okEnabled [
	^ self info canMakePackage
]

{ #category : #accessing }
MCSliceMaker >> okToDoSlice [
	^ okToDoSlice ifNil: [false]
]

{ #category : #accessing }
MCSliceMaker >> okToDoSlice: aBoolean [
	okToDoSlice := aBoolean
]

{ #category : #'user interface' }
MCSliceMaker >> openFor: anotherWindow [
	window := self window.
	anotherWindow openModal: window.
	^ self resultInfo
]

{ #category : #accessing }
MCSliceMaker >> resultInfo [
	^ self okToDoSlice 
		ifTrue: [info]
]

{ #category : #'user interface' }
MCSliceMaker >> rootItems [
	^ (MCWorkingCopy allManagers sort: [ :a :b | 
		"Sort alphabetically, putting modified packages first in the list. Copy/pasted from MCWorkingCopyBrowser>>#orderSpecs"
		(a modified = b modified) 
			ifTrue: [ a package name <= b package name  ]
			ifFalse: [ a modified ]
		])
]

{ #category : #selecting }
MCSliceMaker >> selectionChanged: aSelectionChangedAnnounce [
	| selectedWorkingCopies |
	selectedWorkingCopies := aSelectionChangedAnnounce selection selectedItems.
	self info includedPackages: selectedWorkingCopies.
	self changed: #okEnabled
]

{ #category : #updating }
MCSliceMaker >> update: aSymbol [
	aSymbol = #issueNumber ifTrue: [ self downloadIssueTitle ].
	self changed: #okEnabled.
	super update: aSymbol
]

{ #category : #'user interface' }
MCSliceMaker >> window [
	| sliceInfoRow buttonsRow okBtn cancelBtn issueNumberEntry issueSummaryEntry tree treeMorph |
	window := StandardWindow new model: self.
	window title: ' Slice Maker' translated.
	issueNumberEntry := self issueNumberEntryOn: window.
	issueSummaryEntry := self issueTitleOn: window.
	sliceInfoRow := PanelMorph new
		changeTableLayout;
		cellInset: 10 @ 4;
		layoutInset: 4 @ 4;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	sliceInfoRow
		addAllMorphs:
			{issueNumberEntry.
			(window newLabel: '-').
			issueSummaryEntry}.
	buttonsRow := PanelMorph new
		changeTableLayout;
		cellInset: 10 @ 4;
		layoutInset: 4 @ 4;
		listDirection: #rightToLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	okBtn := window newOKButtonFor: self getEnabled: #okEnabled.
	cancelBtn := window newCancelButtonFor: self.
	okBtn isDefault: true.
	self info addDependent: okBtn.
	buttonsRow
		addAllMorphs:
			{cancelBtn.
			okBtn}.
	window addMorph: sliceInfoRow fullFrame: 
		(LayoutFrame identity bottomFraction: 0; bottomOffset: sliceInfoRow height).
	tree := MorphTreeModel new
		rootItems: self rootItems;
		beCheckList;
		autoMultiSelection: true;
		wrapBlockOrSelector: #packageNameWithStar;
		headerLabel: 'Dirty Packages to be included in the slice';
		yourself.
	tree onSelectionChangeSend: #selectionChanged: to: self.
	treeMorph := tree defaultTreeMorph
		buildContents;
		vResizing: #spaceFill;
		yourself.	"selectAll; "	"hResizing: #spaceFill;"
	window
		addMorph: treeMorph
		fullFrame:
			(LayoutFrame identity
				topOffset: sliceInfoRow height;
				bottomOffset: buttonsRow height negated).
	window addMorph: buttonsRow fullFrame: 
		(LayoutFrame identity topFraction: 1; 
		topOffset: buttonsRow height negated).
	^ window
]
