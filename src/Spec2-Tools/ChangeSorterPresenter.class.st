"
A change sorter is a little tool to manage changes. 
Developer can give name to change containers and all the change made during this container is active are collected in the container. 
The DualChangeSorter tool supports manipulation of changes between containers and fileout of the changes.

ChangeSorterPresenter new openWithSpec
"
Class {
	#name : #ChangeSorterPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'model',
		'changesListPresenter',
		'methodsListPresenter',
		'classesListPresenter',
		'textPresenter'
	],
	#category : #'Spec2-Tools-ChangeSorter'
}

{ #category : #specs }
ChangeSorterPresenter class >> defaultSpec [
	<spec>
	^ SpPanedLayout newVertical
		position: 35 percent;
		add:
			(SpPanedLayout newHorizontal
				add: #changesListPresenter;
				add: #classesListPresenter;
				yourself);
		add:
			(SpPanedLayout newVertical
				add: #methodsListPresenter;
				add: #textPresenter;
				yourself)
]

{ #category : #specs }
ChangeSorterPresenter class >> title [
	^ 'Change sorter'
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> addPreamble [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self model addPreambleTo: self selectedChangeSet.
	changesListPresenter setSelectedItem: self selectedChangeSet
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> browseChangeSet [
	"Open a message list browser on methods in the current change set"

	| messages |
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	messages := self selectedChangeSet changedMessageList select: [ :each | each isValid ].
	Smalltalk tools messageList browse: messages title: 'Methods in Change Set ' , self selectedChangeSet name
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> browseImplementorsOfMessages [

	self model browseMessagesFrom: self selectedSelector
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	self selectedClass ifNotNil:
		[:myClass | Smalltalk tools browser openOnClass: myClass selector: self selectedSelector ]
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> browseSendersOfMessages [

	self model browseSendersOfMessagesFrom: self selectedSelector
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> browseVersions [
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	
	self selectedClass ifNotNil: [:class |
		self selectedSelector ifNotNil: [:selector || method |
			method := self createRingMethodForSelector: selector inClass: class.
			self model browseVersionsFrom: method ]]
]

{ #category : #menu }
ChangeSorterPresenter >> changeSetMenu [
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	^ self newMenu
		title: 'Change Set';
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Make changes go to me';
						shortcut: $m meta;
						action: [ self setCurrentChangeSet ] ];
				addItem: [ :anItem | 
					anItem
						name: 'New change set...';
						shortcut: $n meta;
						action: [ self newSet ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Find...';
						shortcut: $f meta;
						action: [ self findChangeSet ] ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Rename change set';
						shortcut: $r meta;
						action: [ self rename ] ];
				addItem: [ :anItem | 
					anItem
						name: 'File out';
						shortcut: $o meta;
						action: [ self fileOut ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Browse methods';
						shortcut: $b meta;
						action: [ self browseChangeSet ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Browse change set';
						shortcut: $B meta;
						action: [ self openChangeSetBrowser ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Add/Edit preamble';
						shortcut: $p meta;
						action: [ self addPreamble ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove preamble';
						action: [ self removePreamble ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Add/Edit postscript...';
						action: [ self editPostscript ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove postscript';
						action: [ self removePostscript ] ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Destroy change set';
						shortcut: $x meta;
						action: [ self remove ] ] ];
		yourself
]

{ #category : #api }
ChangeSorterPresenter >> changeSets [

	^ self changesListPresenter listItems
]

{ #category : #accessing }
ChangeSorterPresenter >> changesListPresenter [

	^ changesListPresenter
]

{ #category : #api }
ChangeSorterPresenter >> changesMenu: aBlock [
	changesListPresenter contextMenu: aBlock
]

{ #category : #api }
ChangeSorterPresenter >> changesPresenterOn: aShortcut do: aBlock [

	changesListPresenter bindKeyCombination: aShortcut toAction: aBlock
]

{ #category : #menu }
ChangeSorterPresenter >> classMenu [
	"Fill aMenu with items appropriate for the class list"

	^ self newMenu
		title: 'Class list';
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Delete class from change set';
						shortcut: $d meta;
						action: [ self forgetClass ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove class from system';
						shortcut: $x meta;
						action: [ self removeClass ] ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Browse full';
						shortcut: $b meta;
						action: [ self browseMethodFull ] ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'FileOut';
						action: [ self fileOutClass ] ] ];
		yourself
]

{ #category : #shortcuts }
ChangeSorterPresenter >> classShortcuts: event from: aMorph [

	self selectedClass
		ifNotNil:[
			event keyString = '<Cmd-b>' 		ifTrue: [ ^ self browseMethodFull ].
			event keyString = '<Cmd-d>' 		ifTrue: [ ^ self forgetClass ].
			event keyString = '<Cmd-x>' 		ifTrue: [ ^ self removeClass ]].
]

{ #category : #accessing }
ChangeSorterPresenter >> classesListPresenter [

	^ classesListPresenter
]

{ #category : #api }
ChangeSorterPresenter >> classesMenu: aBlock [
	classesListPresenter contextMenu: aBlock
]

{ #category : #api }
ChangeSorterPresenter >> classesOn: aShortcut do: aBlock [

	classesListPresenter bindKeyCombination: aShortcut toAction: aBlock
]

{ #category : #initialization }
ChangeSorterPresenter >> connectPresenters [
	self registerChangeActions.
	self registerClassActions.
	self registerSelectorActions.

	changesListPresenter items
		ifNotEmpty: [ changesListPresenter selectIndex: 1 ]
]

{ #category : #private }
ChangeSorterPresenter >> createRingMethodForRemovedSelector: aSelector inClass: aClass [
	"Create RingMethod from the	removealinfo"

	| removalInfo method |
	method := RGMethodDefinition class: aClass selector: aSelector.
	removalInfo := (self selectedChangeSet changeRecorderFor: aClass) methodChanges at: aSelector ifAbsent: [ ^ method ].
	method sourcePointer: removalInfo methodInfoFromRemoval first.
	^ method
]

{ #category : #private }
ChangeSorterPresenter >> createRingMethodForSelector: aSelector inClass: aClass [
	"Create RingMethod from the class compiledMethod or if it was removed from the
	removealinfo"
	(aClass includesSelector: aSelector) ifTrue:[ ^ aClass compiledMethodAt: aSelector].
	^ self createRingMethodForRemovedSelector: aSelector inClass: aClass.
]

{ #category : #api }
ChangeSorterPresenter >> currentText [

	 ^ textPresenter getText
]

{ #category : #private }
ChangeSorterPresenter >> defaultTitle [
	
	^  'Change Sorter'
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> editPostscript [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self selectedChangeSet editPostscript
]

{ #category : #shortcuts }
ChangeSorterPresenter >> ensureKeyBindingsFor: aWidget [

	super ensureKeyBindingsFor: aWidget.
	
	self registerChangeSetShortcuts: changesListPresenter.
	self registerClassShortcuts: classesListPresenter.
	self registerMethodShortcuts: methodsListPresenter.
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> fileOut [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self selectedChangeSet fileOut
]

{ #category : #'menu - class' }
ChangeSorterPresenter >> fileOutClass [

	self model fileOutClass: self selectedClass from: self selectedChangeSet
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> fileOutMessage [
	"Put a description of the selected message on a file"

	self selectedSelector ifNotNil: [:selector |
		Cursor write showWhile:
			[ self model fileOutSelector: selector from: self selectedClass ]]
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> findChangeSet [

	| set |
	set := self model findChangeSetIn: self changeSets.
	changesListPresenter setSelectedItem: set.
]

{ #category : #private }
ChangeSorterPresenter >> forceChangesListRefresh [
	| sel |
	
	sel := self selectedChangeSet.
	changesListPresenter items: self model allChanges.
	changesListPresenter setSelectedItem: sel.
]

{ #category : #'menu - class' }
ChangeSorterPresenter >> forgetClass [
	"Remove all mention of this class from the changeSet"

	self okToChange ifFalse: [^ self].
	self selectedClass ifNotNil: [:class |
		self model removeClass: class from: self selectedChangeSet.
		self setSelectedChangeSet: self selectedChangeSet ].

]

{ #category : #'menu - message' }
ChangeSorterPresenter >> forgetMessage [

	self okToChange ifFalse: [^ self].
	self selectedSelector ifNotNil: [:selector | 
		self model 
			forgetSelector: selector 
			inClass: self selectedClass 
			fromChangeSet: self selectedChangeSet.
		self updateClassesList.
		self setSelectedClassIndex: self selectedClassIndex ]
]

{ #category : #initialization }
ChangeSorterPresenter >> initialize [
	model := ChangeSorterModel new.
	SystemAnnouncer uniqueInstance weak when: CurrentChangeSetChanged send: #updateTitle to: self.
	super initialize.
	self initializeAnnouncements
]

{ #category : #initialization }
ChangeSorterPresenter >> initializeAnnouncements [
	SystemAnnouncer uniqueInstance weak 
		when:
			ClassAdded , ClassCommented , ClassRecategorized , ClassModifiedClassDefinition , ClassRemoved , ClassRenamed
				, ClassReorganized , MethodAdded , MethodModified , MethodRecategorized , MethodRemoved , ProtocolAdded
				, ProtocolRemoved
		send: #updateClassesList
		to: self
]

{ #category : #initialization }
ChangeSorterPresenter >> initializePresenters [
	methodsListPresenter := self newList.
	classesListPresenter := self newList.
	changesListPresenter := self newList.
	textPresenter := self newCode.
	self setFocus.
	methodsListPresenter contextMenu: self messageMenu.
	changesListPresenter contextMenu: self changeSetMenu.
	classesListPresenter contextMenu: self classMenu.
	methodsListPresenter enableItemSubstringFilter.
	classesListPresenter enableItemSubstringFilter.
	changesListPresenter enableItemSubstringFilter.
	changesListPresenter items: self model allChanges.
	changesListPresenter display: [ :item | item name ].
	classesListPresenter sortingBlock: [ :a :b | a name < b name ]
]

{ #category : #initialization }
ChangeSorterPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: self title;
		initialExtent: 500 @ 350
]

{ #category : #menu }
ChangeSorterPresenter >> messageMenu [
	"Build a menu with items appropriate for the message list; could be for a single or double changeSorter"

	^ self newMenu
		title: 'Message list';
		addGroup: [ :group | 
			group
				addItem: [ :anItem | 
					anItem
						name: 'Delete method from changeSet';
						shortcut: $d meta;
						action: [ self forgetMessage ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove method from system';
						shortcut: $x meta;
						action: [ self removeMessage ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :anItem | 
					anItem
						name: 'Browse full';
						shortcut: $b meta;
						action: [ self browseMethodFull ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :anItem | 
					anItem
						name: 'FileOut';
						action: [ self fileOutMessage ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Senders of...';
						shortcut: $n meta;
						action: [ self browseSendersOfMessages ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Implementors of...';
						shortcut: $m meta;
						action: [ self browseImplementorsOfMessages ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Versions';
						shortcut: $v meta;
						action: [ self browseVersions ] ] ];
		yourself
]

{ #category : #shortcuts }
ChangeSorterPresenter >> methodShortcuts: event from: aMorph [

	self selectedSelector
		ifNotNil:[
			event keyString = '<Cmd-b>' 		ifTrue: [ ^ self browseMethodFull ].
			event keyString = '<Cmd-d>' 		ifTrue: [ ^ self forgetMessage ].
			event keyString = '<Cmd-m>' 	ifTrue: [ ^ self browseImplementorsOfMessages ].
			event keyString = '<Cmd-n>' 		ifTrue: [ ^ self browseSendersOfMessages ].
			event keyString = '<Cmd-v>' 		ifTrue: [ ^ self browseVersions ].
			event keyString = '<Cmd-x>' 		ifTrue: [ ^ self removeMessage ]].
]

{ #category : #accessing }
ChangeSorterPresenter >> methodsListPresenter [

	^ methodsListPresenter
]

{ #category : #api }
ChangeSorterPresenter >> methodsMenu: aBlock [
	methodsListPresenter contextMenu: aBlock
]

{ #category : #api }
ChangeSorterPresenter >> methodsOn: aShortcut do: aBlock [

	methodsListPresenter bindKeyCombination: aShortcut toAction: aBlock
]

{ #category : #accessing }
ChangeSorterPresenter >> model [

	^ model
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> newSet [
	| aSet |
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self okToChange ifFalse: [ ^ self ].
	aSet := self model createNewSet.
	aSet
		ifNotNil: [ self updateChangesList.
			changesListPresenter setSelectedItem: aSet.
			self updateWindowTitle ]
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> openChangeSetBrowser [
	| cs |
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	cs := self selectedChangeSet ifNil: [ ^ self ].
	^ Smalltalk tools messageList browseClasses: (RBBrowserEnvironment new forClasses: cs changedClasses)
]

{ #category : #initialization }
ChangeSorterPresenter >> registerChangeActions [
	changesListPresenter
		whenSelectionChangedDo: [ :selection | 
			self updateTextContents.
			selection selectedItem
				ifNil: [ classesListPresenter items: {}.
					methodsListPresenter items: {} ]
				ifNotNil: [ :change | 
					classesListPresenter unselectAll.
					classesListPresenter items: change changedClasses ] ]
]

{ #category : #shortcuts }
ChangeSorterPresenter >> registerChangeSetShortcuts: aWidget [

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self selectedChangeSet ifNotNil:[ self browseChangeSet ]].
	aWidget bindKeyCombination: $b meta shift toAction: [ self selectedChangeSet ifNotNil:[ self openChangeSetBrowser ]].
	aWidget bindKeyCombination: $m meta toAction: [ self selectedChangeSet ifNotNil:[ self setCurrentChangeSet ]].
	aWidget bindKeyCombination: $n meta toAction: [ self selectedChangeSet ifNotNil:[ self newSet ]].
	aWidget bindKeyCombination: $o meta toAction: [ self selectedChangeSet ifNotNil:[ self fileOut ]].
	aWidget bindKeyCombination: $r meta toAction: [ self selectedChangeSet ifNotNil:[ self rename ]].
	aWidget bindKeyCombination: $p meta toAction: [ self selectedChangeSet ifNotNil:[ self addPreamble ]].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self selectedChangeSet ifNotNil:[ self remove ]].
	aWidget bindKeyCombination: $f meta toAction: [ self findChangeSet ].
		
]

{ #category : #initialization }
ChangeSorterPresenter >> registerClassActions [
	classesListPresenter
		whenSelectionChangedDo: [ :selection | 
			self updateTextContents.
			textPresenter behavior: selection selectedItem.
			textPresenter syntaxHighlight: false.
			selection selectedItem
				ifNil: [ methodsListPresenter items: {} ]
				ifNotNil: [ :class | 
					methodsListPresenter unselectAll.
					methodsListPresenter items: (self selectedChangeSet selectorsInClass: class name) sort ] ]
]

{ #category : #shortcuts }
ChangeSorterPresenter >> registerClassShortcuts: aWidget [

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self selectedClass ifNotNil:[ self browseMethodFull ]].
	aWidget bindKeyCombination: $d meta toAction: [ self selectedClass ifNotNil:[ self forgetClass ]].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self selectedClass ifNotNil:[ self removeClass ]].
]

{ #category : #shortcuts }
ChangeSorterPresenter >> registerMethodShortcuts: aWidget [

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self selectedSelector ifNotNil:[ self browseMethodFull ]].
	aWidget bindKeyCombination: $d meta toAction: [ self selectedSelector ifNotNil:[ self forgetMessage ]].
	aWidget bindKeyCombination: PharoShortcuts current browseImplementorsShortcut toAction: [ self selectedSelector ifNotNil:[ self browseImplementorsOfMessages ]].
	aWidget bindKeyCombination: PharoShortcuts current browseSendersShortcut toAction: [ self selectedSelector ifNotNil:[ self browseSendersOfMessages ]].
	aWidget bindKeyCombination: PharoShortcuts current browseVersionsShortcut toAction: [ self selectedSelector ifNotNil:[ self browseVersions ]].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self selectedSelector ifNotNil:[ self removeMessage ]].
]

{ #category : #initialization }
ChangeSorterPresenter >> registerSelectorActions [
	methodsListPresenter whenSelectionChangedDo: [ :selection | self updateTextContents ]
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> remove [
	"Completely destroy my change set.  Check if it's OK first"

	self okToChange ifFalse: [^ self].
	self model removeChangeSet: self selectedChangeSet prompting: true.
	self updateChangesList.
	changesListPresenter resetSelection
]

{ #category : #'menu - class' }
ChangeSorterPresenter >> removeClass [
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	(self model removeClass: self selectedClass)
		ifTrue: [ self setSelectedChangeSet: self selectedChangeSet ]
]

{ #category : #'menu - message' }
ChangeSorterPresenter >> removeMessage [
	"Remove the selected message from the system."
	
	self okToChange
		ifFalse: [^ self].
	self selectedSelector
		ifNotNil: [:selector | | class method | 
			class := self selectedClass.
				(class includesSelector: selector) ifFalse:[^ self].
				method := class>>selector.
				(self model removeMethod: method inClass: class)
					ifTrue: [ self updateMessagesList ]]
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> removePostscript [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self model removePostscriptFrom: self selectedChangeSet.
	changesListPresenter setSelectedItem: self selectedChangeSet
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> removePreamble [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self selectedChangeSet hasPreamble ifFalse: [ ^ self inform: 'Change set has no preamble' ].
	self selectedChangeSet removePreamble.
	changesListPresenter setSelectedItem: self selectedChangeSet
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> rename [
	| set |
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	set := self model rename: self selectedChangeSet.

	changesListPresenter
		updateList;
		setSelectedItem: set.
	self updateWindowTitle
]

{ #category : #api }
ChangeSorterPresenter >> selectedChangeSet [
	^ changesListPresenter selection selectedItem
]

{ #category : #api }
ChangeSorterPresenter >> selectedChangeSetIndex [

	 ^ changesListPresenter selectedIndex
]

{ #category : #api }
ChangeSorterPresenter >> selectedClass [
	^ classesListPresenter selection selectedItem
]

{ #category : #api }
ChangeSorterPresenter >> selectedClassIndex [

	 ^ classesListPresenter selectedIndex
]

{ #category : #api }
ChangeSorterPresenter >> selectedSelector [
	^ methodsListPresenter selection selectedItem
]

{ #category : #api }
ChangeSorterPresenter >> selectorsMenu: aBlock [

	methodsListPresenter menu: aBlock
]

{ #category : #'menu - change set' }
ChangeSorterPresenter >> setCurrentChangeSet [
	self selectedChangeSet ifNil: [ ^ self inform: 'No change set selected' ].
	self model setCurrentChangeSet: self selectedChangeSet.
	self updateWindowTitle
]

{ #category : #initialization }
ChangeSorterPresenter >> setFocus [

	self focusOrder
		add: changesListPresenter;
		add: classesListPresenter;
		add: methodsListPresenter;
		add: textPresenter.
]

{ #category : #api }
ChangeSorterPresenter >> setSelectedChangeSet: aChangeSet [

	 ^ changesListPresenter setSelectedItem: aChangeSet
]

{ #category : #api }
ChangeSorterPresenter >> setSelectedChangeSetIndex: anIndex [

	 ^ changesListPresenter setSelectedIndex: anIndex
]

{ #category : #api }
ChangeSorterPresenter >> setSelectedClass: aClass [

	 ^ classesListPresenter setSelectedItem: aClass
]

{ #category : #api }
ChangeSorterPresenter >> setSelectedClassIndex: anIndex [

	 ^ classesListPresenter setSelectedIndex: anIndex
]

{ #category : #accessing }
ChangeSorterPresenter >> textPresenter [

	^ textPresenter
]

{ #category : #api }
ChangeSorterPresenter >> title [
	^ String streamContents: [ :stream | stream << self class title << ' on: ' << self model currentChangeSet name ]
]

{ #category : #api }
ChangeSorterPresenter >> updateChangesList [
	| sel |
	sel := self selectedChangeSet.
	changesListPresenter items: self model allChanges.
	changesListPresenter selectItem: sel
]

{ #category : #api }
ChangeSorterPresenter >> updateClassesList [
	| sel |
	sel := self selectedClass.
	self selectedChangeSet
		ifNil: [ classesListPresenter items: {} ]
		ifNotNil: [ :change | 
			classesListPresenter
				items: (change changedClasses sort: [ :a :b | a name < b name ]) ].
	sel ifNotNil: [ classesListPresenter selectItem: sel ]
]

{ #category : #api }
ChangeSorterPresenter >> updateClassesListAndMessagesList [
	| sel |
	sel := methodsListPresenter selectedItem.
	self updateClassesList.
	methodsListPresenter setSelectedItem: sel.
	self updateMessagesList
]

{ #category : #api }
ChangeSorterPresenter >> updateMessagesList [
	| sel |
	sel := methodsListPresenter selectedItem.
	
	self selectedClass
			ifNil: [ methodsListPresenter items: {} ]
			ifNotNil: [:class | methodsListPresenter items: (self selectedChangeSet selectorsInClass: class name) sort].
				
	methodsListPresenter setSelectedItem: sel
]

{ #category : #private }
ChangeSorterPresenter >> updateTextContents [
	| text |
	text := self model setContentsOfChangeSet: self selectedChangeSet forClass: self selectedClass andSelector: self selectedSelector.
	(text asString lines
		anySatisfy: [ :line | 
			self model classDescriptionsMap values , {'Method was added, but cannot be found!' . 'Added then removed (see versions)' . 'Method has been removed (see versions)' . 'Class organization was changed.'}
				includes: line ]) ifTrue: [ textPresenter withoutSyntaxHighlight ] ifFalse: [ textPresenter withSyntaxHighlight ].
	textPresenter text: text
]

{ #category : #api }
ChangeSorterPresenter >> updateWindowTitle [
	self withWindowDo: [ :window | window title: self title ]
]

{ #category : #'api-events' }
ChangeSorterPresenter >> whenChangesListChanges: aBlock [
	changesListPresenter whenModelChangedDo: aBlock
]
