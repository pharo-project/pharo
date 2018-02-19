Class {
	#name : #KomitterUI,
	#superclass : #ComposablePresenter,
	#instVars : [
		'author',
		'authorName',
		'commitMessage',
		'diff',
		'message',
		'tree',
		'remotes',
		'remoteLabel',
		'checkCommit',
		'model',
		'previousMessages',
		'canCommit',
		'manageRemotes'
	],
	#classInstVars : [
		'manageRemotesIcon'
	],
	#category : #'Komitter-UI'
}

{ #category : #specs }
KomitterUI class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :topHalf |
			topHalf
				add: #tree;
				addVSplitter;
				newColumn: [ :c ] width: 4;
				newColumn: [ :commitInfoColumn |
					commitInfoColumn
						newRow: [ :commitRow | 
							commitRow 
								add: #commitMessage;
								add: #previousMessages width: self buttonHeight
						] height: self buttonHeight;
						add: #message;
						newRow: [ :authorRow |
							authorRow 
								add: #authorName width: 50;
								add: #author
						] height: self inputTextHeight +4 ;
						newRow: [ :repoRow |
							repoRow 
								add: #remoteLabel width: 50;
								add: #remotes;
								add: #manageRemotes width: self buttonHeight
						] height: self buttonHeight
				]
		] bottom: 0.5;
		addHSplitter;
		newRow: [ :row | row add: #diff ] top: 0.5;
		yourself
]

{ #category : #icons }
KomitterUI class >> historyIcon [
	^ self iconNamed: #historyIcon
]

{ #category : #'private-icons' }
KomitterUI class >> historyIconData [

	^ #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4286673726 4286673726 4286673726 4286673726 4286673726 0 0 0 4278217148 4278217148 4278217148 4278217148 4286673726 4286673726 0 0 4288516712 4294967295 4294967295 4294967295 4294967295 4286673726 4286673726 4278217148 4286104319 4286104319 4286104319 4278217148 4294967295 4288516712 4284839308 0 4288516712 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4278217148 4286104319 4286104319 4286104319 4278217148 4294967295 4288516712 4284839308 0 4288253803 4294967295 4291942109 4291482074 4294967295 4294967295 4294967295 4278217148 4286104319 4286104319 4286104319 4278217148 4294967295 4288253803 4284839308 0 4287925358 4294967295 4294967295 4294967295 4290759126 4290759126 4294967295 4278217148 4286038527 4286038527 4286039039 4278217148 4294967295 4287925358 4284707465 0 4287465586 4294705151 4291942109 4291482074 4294705151 4294705151 4294705151 4278216120 4285971967 4285971967 4285971967 4278216633 4294705151 4287465586 4284444294 0 4287005814 4294376959 4294376959 4294376959 4290759126 4290759126 4294376959 4278215349 4285840127 4285840127 4285840127 4278215862 4294376959 4287005814 4284115330 0 4286480763 4294114047 4291942109 4291482074 4294114047 4294114047 4294114047 4278214064 4285708287 4285643519 4285708799 4278214319 4294114047 4286480763 4283786365 0 4286086271 4293785343 4293785343 4293785343 4290759126 4290759126 4293785343 4278213034 4285641983 4285641983 4285641983 4278213034 4293785343 4286086271 4283391608 0 4285626499 4293522687 4293522687 4293522687 4293522687 4293522687 4293522687 4278211492 4285444351 4285378815 4285378815 4278212003 4293457151 4285626499 4283062642 0 4285298055 4285298055 4285298055 4285298055 4285298055 4293325311 4293325311 4278210466 4285116671 4285116671 4285181951 4278210465 4285298055 4285298055 4282733421 0 0 4282207077 4282207077 4282207077 4285035401 4285035401 4285035401 4278209949 4284918783 4284786943 4284787455 4278209949 4282141286 4282207077 4282207078 0 0 0 0 0 4282141542 4282009699 4282009699 4278209949 4284721919 4278209949 4284525311 4278209949 0 0 0 0 0 0 0 0 0 0 0 4278209949 4278209949 0 4278209949 4278209949 0 0 0 0 0 0 0 0 0 0 0 0 4278209949 0 0 4278209949 0 0 0)
]

{ #category : #'private-icons' }
KomitterUI class >> manageRemotesData [

	^ #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4289496358 4289430823 0 0 0 0 0 0 0 0 0 0 0 0 0 4288970530 4294967295 4294957713 4289035810 0 0 0 0 0 0 0 0 4290811960 4290811960 4292065140 4288509724 4294832859 4294957713 4291666516 4289035810 0 0 0 0 0 0 0 4290549045 4294967295 4294957713 4287918102 4294832859 4294957713 4291666516 4287983638 0 0 0 0 0 0 0 4290285617 4294832859 4294957713 4287523346 4294832859 4294957713 4291666516 4287523089 0 0 0 0 0 0 0 4289956651 4294832859 4294957713 4291473003 4287194382 4294957713 4291666516 4287194382 4290811960 0 0 0 0 0 0 4289561893 4294832859 4294957713 4289562149 0 0 4287194382 4287194382 4294832859 4288706584 0 0 0 0 0 4289232928 4294832859 4294957713 4290946659 0 0 0 4290811960 4294832859 4294957713 4288706584 0 0 0 0 0 4288903963 4294957713 4289496358 4289430823 0 0 4290482995 4294832859 4294957713 4288706584 0 0 0 0 0 0 4290617694 4288970530 4294967295 4294957713 4289035810 4290022444 4294832859 4294957713 4288706584 0 0 0 0 0 0 0 4288509724 4294832859 4294957713 4291666516 4289035810 4294832859 4294957713 4288706584 0 0 0 0 0 0 0 4287918102 4294832859 4294957713 4291666516 4287983638 4294832859 4294957713 4288706584 0 0 0 0 0 0 0 4287523346 4294832859 4294957713 4291666516 4287523089 4288706584 4288706584 4288706584 0 0 0 0 0 0 0 0 4287194382 4294957713 4291666516 4287194382 0 0 0 0 0 0 0 0 0 0 0 0 0 4287194382 4287194382 0 0 0 0 0 0 0 0 0 0 0 0)
]

{ #category : #icons }
KomitterUI class >> manageRemotesIcon [

	^ manageRemotesIcon ifNil: [ manageRemotesIcon := Smalltalk ui icons form16x16FromContents: self manageRemotesData ]
]

{ #category : #icons }
KomitterUI class >> taskbarIconName [

	^ Komitter taskbarIconName
]

{ #category : #api }
KomitterUI >> addRoot: aPackage [
	| aPackageNode |
	
	aPackageNode := KomitPackageNode new content: aPackage.
	
	tree roots: (tree roots
		add: aPackageNode;
		sorted)
]

{ #category : #accessing }
KomitterUI >> author [
	^ author
]

{ #category : #accessing }
KomitterUI >> authorName [
	^ authorName
]

{ #category : #api }
KomitterUI >> cancel [

	message accept.
	Komitter lastMessage: message text
]

{ #category : #accessing }
KomitterUI >> checkCommit [
	^ checkCommit
]

{ #category : #private }
KomitterUI >> chooseFromPreviousMessages [
	| list index |
	
	list := MCSaveVersionDialog previousMessages collect: [:s | s truncateWithElipsisTo: 80 ].
	list ifEmpty: [ 
		UIManager default inform: 'No previous log message was entered'. 
		^ self ].
	
	index := UIManager default chooseFrom: list.
	
	"no comment was selected"
	index isZero ifTrue: [ ^ self ].

	message 
		text: (MCSaveVersionDialog previousMessages at: index);
		takeKeyboardFocus
]

{ #category : #accessing }
KomitterUI >> commitMessage [

	^ commitMessage
]

{ #category : #private }
KomitterUI >> commitableEntities [
	| map |
	
	map := Dictionary new.
	
	tree roots do: [ :e || nodes | 
		e content isFullyCommited: e selected.
		nodes := e allSelectedItems
			collect: [ :each | each content ]
			thenSelect: [ :each | each isCommitable ].
		nodes ifNotEmpty: [ map at: e content put: nodes ] ].

	^ map
]

{ #category : #accessing }
KomitterUI >> diff [
	^ diff
]

{ #category : #'private-icons' }
KomitterUI >> historyIcon [

	^ self class historyIcon
]

{ #category : #api }
KomitterUI >> initialExtent [

	^ (980@740)
]

{ #category : #initialization }
KomitterUI >> initialize [

	model := nil asValueHolder.
	canCommit := false asValueHolder.
	
	super initialize.
	model
		whenChangedDo: [ :m | 
			[ self populateTreeFor: m ] fork.
			message text: m lastMessage.
			self populateRemotes ]
]

{ #category : #initialization }
KomitterUI >> initializeDialogWindow: aWindow [

	self bindKeyCombination: (Character cr control unix | $m control mac) toAction: [ aWindow triggerOkAction ].

	aWindow toolbar okButton enabled: canCommit value.

	canCommit whenChangedDo: [ :b | aWindow toolbar okButton enabled: b ].
	
	self focusOrder
		removeAll;
		add: message;
		add: remotes;
		add: aWindow toolbar;
		add: tree;
		add: previousMessages
]

{ #category : #initialization }
KomitterUI >> initializeLatelyPresenter [
	tree
		whenHighlightedItemChanged:
			[ :node | 
			| item |
			item := node ifNil: [ nil ] ifNotNil: [ node content ].
			self refreshDiffForItem: item ].
	tree whenSelectedItemsChanged: [ self updateRemotes ]
]

{ #category : #initialization }
KomitterUI >> initializeWidgets [

	tree := self newTree.
	diff := self instantiate: DiffPresenter.
	message := self newText.
	author := self newTextInput.
	authorName := self newLabel.
	commitMessage := self newLabel.
	remotes := self newDropList.
	remoteLabel := self newLabel.
	checkCommit := self newCheckBox.
	previousMessages := self newButton.
	manageRemotes := self newButton.
	
	tree 
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		columns: { TreeColumnPresenter new
			displayBlock: [:node | node label ] }.
			
	diff
		showOptions: false;
		leftText: 'No selection for diff'.

	authorName label: 'Author:'.
	commitMessage label: 'Commit message:'.
	remoteLabel label: 'Remote:'.
	
	checkCommit
		label: 'Check Lint rules (NOT YET WORKING)';
		labelClickable: true;
		state: false;
		enabled: false.	
		
	author 
		text: Author fullNamePerSe;
		autoAccept: true.
		
	previousMessages
		icon: self historyIcon;
		action: [ self chooseFromPreviousMessages ];
		bindKeyCombination: Character space asKeyCombination 
		toAction: [ previousMessages action value ];
		help: 'Previous messages'.
		
	message
		bindKeyCombination: Character tab asKeyCombination
		toAction: [ self giveFocusToNextFrom: message ];
		bindKeyCombination: Character tab shift
		toAction: [ self giveFocusToPreviousFrom: message ];
		wantsVisualFeedback: false.
	
	manageRemotes
		icon: self manageRemotesIcon;
		action: [ self openRemoteManager ];
		help: 'Open the remotes manager'.
	
	self whenBuiltDo: [ 
		checkCommit labelOnLeft.
		diff showOnlySource: true.
		message selectAll.
		self updateRemotes ].
]

{ #category : #api }
KomitterUI >> komit [
	"Gather all the data and create a Komit object out of them"
	
	^ remotes selectedItem komitOn: self
]

{ #category : #private }
KomitterUI >> leftTextFor: aKOObject [

	aKOObject ifNil: [ ^ '' ].
	^ aKOObject koSourceText
]

{ #category : #accessing }
KomitterUI >> manageRemotes [
	^ manageRemotes
]

{ #category : #'private-icons' }
KomitterUI >> manageRemotesIcon [

	^ self class manageRemotesIcon
]

{ #category : #accessing }
KomitterUI >> message [

	^ message
]

{ #category : #api }
KomitterUI >> model [

	^ model value
]

{ #category : #api }
KomitterUI >> model: aModel [

	model value: aModel
]

{ #category : #api }
KomitterUI >> newKomitFor: aRemote [
	"Gather all the data and create a Komit object out of them"
	
	^ Komit new
		message: message text asString;
		author: author text;
		timestamp: DateAndTime now;
		entities: self commitableEntities;
		unselectedEntities: self uncommittedCommitableEntitied;
		remote: aRemote;
		yourself
]

{ #category : #api }
KomitterUI >> newSliceFor: aRemote [
	"Gather all the data and create a Komit object out of them"
	
	^ Slice new
		message: message text;
		author: author text;
		timestamp: DateAndTime now;
		entities: self commitableEntities;
		unselectedEntities: self uncommittedCommitableEntitied;
		remote: aRemote;
		yourself
]

{ #category : #private }
KomitterUI >> openRemoteManager [
	| view |
	
	view := PackageRemotesManager new packages: (tree roots collect: [ :e | self remoteNodeFor: e ]).
	view openDialogWithSpec
		centered;
		modalRelativeTo: self window
]

{ #category : #initialization }
KomitterUI >> populateRemotes [
	
	remotes 
		items: self model remotes asOrderedCollection;
		displayBlock: [ :each | each label ];
		iconHolder: [ :each | each icon ]
]

{ #category : #initialization }
KomitterUI >> populateRemotesFor: aCollection [
	
	remotes 
		items: (self model remotesFor: aCollection) asOrderedCollection;
		displayBlock: [ :each | each label ];
		iconHolder: [ :each | each icon ]
]

{ #category : #initialization }
KomitterUI >> populateTreeFor: newModel [
	| roots |
	roots := newModel stagingArea 
		collect: [ :each | KomitPackageNode new content: each]
		thenSelect: [:each | each isEmpty not ].
	tree
		beCheckList;
		roots: roots asOrderedCollection sorted;
		expandAll;
		collapseAll;
		expandRoots.
	self initializeLatelyPresenter.
	canCommit value: true
]

{ #category : #accessing }
KomitterUI >> previousMessages [

	^ previousMessages
]

{ #category : #private }
KomitterUI >> refreshDiffForItem: item [
	diff
		contextClass:
			(item
				ifNotNil: [ 
					item isMethod
						ifTrue: [ item operation targetClass ]
						ifFalse: [ nil ] ]).
	diff
		leftText: (self leftTextFor: item);
		rightText: (self rightTextFor: item).
	^ (item isNil or: [ item added ])
		ifTrue: [ diff showOnlyDestination: true ]
		ifFalse:
			[ 
			(item removed or: [ item isPackage or: [ item isKomitClass and: [ item isDirty not ] ] ])
				ifTrue: [ diff showOnlySource: true ]
				ifFalse: [ diff showBoth ] ]
]

{ #category : #accessing }
KomitterUI >> remoteLabel [

	^ remoteLabel
]

{ #category : #private }
KomitterUI >> remoteNodeFor: e [

	^ TreeNodePresenter new
		content: e content package workingCopy
]

{ #category : #accessing }
KomitterUI >> remotes [

	^ remotes
]

{ #category : #api }
KomitterUI >> removeRoot: aKomitPackage [
	| roots highlight |
	
	highlight := tree highlightedItem.
	
	roots := tree roots.
	roots
		remove: (KomitPackageNode new
			content: aKomitPackage;
			yourself).
		
	tree roots: roots.
	highlight takeHighlight
]

{ #category : #private }
KomitterUI >> rightTextFor: aKOObject [

	aKOObject ifNil: [ ^ '' ].
	^ aKOObject koDestinationText
]

{ #category : #api }
KomitterUI >> roots: aCollection [

	tree roots: aCollection
]

{ #category : #api }
KomitterUI >> title [

	^ 'Commit changes'
]

{ #category : #accessing }
KomitterUI >> tree [
	^ tree
]

{ #category : #private }
KomitterUI >> uncommittedCommitableEntitied [
	| map |
	
	map := Dictionary new.
	
	tree roots 
		select: [ :e | e isPartialMatch ]
		thenDo: [ :e || nodes | 
			nodes := e allUnselectedItems
				collect: [ :each | each content ]
				thenSelect: [ :each | each isCommitable ].
			nodes ifNotEmpty: [ map at: e content put: nodes ] ].

	^ map
]

{ #category : #private }
KomitterUI >> updateRemotes [
	| roots |
	
	roots := tree roots 
		select: [ :e | e isPartialMatch ]
		thenCollect: [ :e | e content ].
		
	roots
		ifEmpty: [ self populateRemotes ]
		ifNotEmpty: [ self populateRemotesFor: roots ]
]

{ #category : #api }
KomitterUI >> validate [

	self commitableEntities 
		ifEmpty: [ 
			tree takeKeyboardFocus.
			self inform: 'The selection should contain commitable entities'.
			^ false ].
		
	message accept.
	
	^ true
]

{ #category : #private }
KomitterUI >> windowIsClosing [
	super windowIsClosing.
	self model unsubscribe
]
