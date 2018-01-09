"
A MessageBrowser is a UI to browse a list of method, regardless of what they could be.

example: 

MessageBrowser new
	openWithSpec;
	messages: (SystemNavigation new allSendersOf: #at:)
	yourself
"
Class {
	#name : #MessageBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'listModel',
		'textModel',
		'toolbarModel',
		'refreshingBlockHolder',
		'cachedHierarchy',
		'model',
		'topologicSortHolder',
		'textConverterHolder',
		'browsedEnvironment'
	],
	#category : #'Spec-Tools-Senders'
}

{ #category : #'instance creation' }
MessageBrowser class >> browse: aCollection [
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		open
]

{ #category : #'instance creation' }
MessageBrowser class >> browse: aCollection asImplementorsOf: aSymbol [ 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		setRefreshingBlockForImplementorsOf: aSymbol;
		messages: aCollection;
		title: 'Implementors of ', aSymbol printString;
		open
]

{ #category : #'instance creation' }
MessageBrowser class >> browse: aCollection asSendersOf: aSymbol [ 
	"Special Version that sets the correct refreshing Block for Senders Browser"
	^ self new
		setRefreshingBlockForSendersOf: aSymbol;
		autoSelect: aSymbol;
		messages: aCollection;
		title: 'Senders of ', aSymbol printString;
		open
]

{ #category : #'instance creation' }
MessageBrowser class >> browse: aCollection title: aString [
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		title: aString;
		open
]

{ #category : #'instance creation' }
MessageBrowser class >> browse: aCollection title: aString autoSelect: aSelectString [
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		title: aString;
		autoSelect: aSelectString;
		open
]

{ #category : #'instance creation' }
MessageBrowser class >> browseImplementorsOf: aSymbol [ 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self browse: aSymbol implementors asImplementorsOf: aSymbol
]

{ #category : #'instance creation' }
MessageBrowser class >> browseSendersOf: aSymbol [ 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self browse: aSymbol senders asSendersOf: aSymbol
]

{ #category : #specs }
MessageBrowser class >> defaultSpec [
	<spec>
	| delta |
	delta := 13.
	^ SpecLayout composed 
			add: #listModel origin: 0@0 corner: 1@0.5 offsetOrigin: 0@0 offsetCorner: 0@(delta negated);
			add: #toolbarModel origin: 0@0.5 corner: 1@0.5 offsetOrigin: 0@(delta negated) offsetCorner: 0@delta;
			add: #textModel origin: 0@0.5 corner: 1@1 offsetOrigin: 0@delta offsetCorner: 0@0;
			yourself
]

{ #category : #menu }
MessageBrowser class >> messageBrowserListMenu: aBuilder [
	<contextMenu>
	<messageBrowserListMenu>
	| target model message |
	
	target := aBuilder model.
	model := target model.
	target selectedMessage ifNil: [ ^self ].
	
	message := target selectedMessage compiledMethod.
	
	(aBuilder item: #'Browse full...')
		keyText: 'b';
		action: [ 
			Smalltalk tools browser 
				openOnClass: message methodClass
				selector: message selector ].

	(aBuilder item: #'Inspect method...')
		keyText: 'i';
		action: [ message inspect ];
		withSeparatorAfter.

	(aBuilder item: #'Remove method...')
		keyText: 'x';
		action: [ 
			model 
				removeMethod: message 
				inClass: message methodClass ].

	(aBuilder item: #'Senders of...')
		keyText: 'n';
		action: [ model browseSendersOfMessagesFrom: message selector ].

	(aBuilder item: #'Implementors of...')
		keyText: 'm';
		action: [ model browseMessagesFrom: message selector ].

	(aBuilder item: #'Users of...')
		keyText: 'N';
		action: [ model browseClassRefsOf: message methodClass ].
	
	(aBuilder item: #'Versions...')
		keyText: 'v';
		action: [ model browseVersionsFrom: message ].		

]

{ #category : #'instance creation - old' }
MessageBrowser class >> openMessageList: messageList name: aString autoSelect: aSelector refreshingBlockSelector: aRefreshingBlockSelector [
	
	| browser |
	browser := self new
		messages: messageList;
		title: aString;
		autoSelect: aSelector;
		yourself.
		
	aRefreshingBlockSelector ifNotNil:[(browser perform: aRefreshingBlockSelector with: aSelector)].
	^browser open
	
				

]

{ #category : #'tool registration' }
MessageBrowser class >> registerToolsOn: registry [

	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #messageList
]

{ #category : #icons }
MessageBrowser class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#referencesIcon
]

{ #category : #specs }
MessageBrowser class >> testSpec [
	<spec: #default>
	
	^ SpecLayout composed
			newColumn: [:col |
				col 
					add: #listModel;
					addSplitter;
					add: #toolbarModel height: self buttonHeight;
					add: #textModel ]
				
]

{ #category : #specs }
MessageBrowser class >> title [

	^ 'Message Browser'
]

{ #category : #private }
MessageBrowser >> accept: text notifying: notifyer [

	^listModel selectedItem
		ifNotNil: [:message | 
			message methodClass 
				compile: text 
				classified: message protocol
				notifying: notifyer ]
]

{ #category : #'text selection' }
MessageBrowser >> autoSelect: aSelector [
	textModel
		readSelectionBlock:
			[ :text | 
			self selectedMessage ifNotNil:
					[ :msg | (self isMethodDefinition: msg)
						ifFalse: [ self intervalOf: aSelector inCommentText: text ] 
						ifTrue: [ self intervalOf: aSelector inCode: text of: msg ] ] ]
]

{ #category : #actions }
MessageBrowser >> browseClass [
	self currentMethod ifNotNil: [ :method | method methodClass browse ]
]

{ #category : #actions }
MessageBrowser >> browseClassRefs [
	self currentMethod ifNotNil: [ :method | 
		model browseClassRefsOf: method methodClass ]
]

{ #category : #actions }
MessageBrowser >> browseMessages [
	self currentMethod ifNotNil: [ :method |
		 model browseMessagesFrom: method selector ]
]

{ #category : #actions }
MessageBrowser >> browseMethod [
	self currentMethod ifNotNil: [ :method | method browse ]
]

{ #category : #actions }
MessageBrowser >> browseSendersOfMessage [
	self currentMethod ifNotNil: [ :method |
		model browseSendersOfMessagesFrom: method selector ]
]

{ #category : #actions }
MessageBrowser >> browseVersions [
	self currentMethod ifNotNil: [ :method |
		model browseVersionsFrom: method compiledMethod ]
]

{ #category : #testing }
MessageBrowser >> buildHierarchyForMessages: messages [
	| result classes |
	self topologicSort ifFalse: [ 
		result := IdentityDictionary new.
		messages do: [:m | result at: m put: {} ].
		^ result ].
	
	result := Dictionary new.	
	classes := (messages collect: #methodClass) asSet.
	messages do: [:message || level class |
		class := message method methodClass.
		level := OrderedCollection new.
		class allSuperclassesDo: [:superClass |
			(classes includes: superClass)
				ifTrue: [ level addFirst: superClass ]].
		level addLast: class.
		level addLast: message selector.
		result at: message put: level ].
	^ result
]

{ #category : #initialization }
MessageBrowser >> buildUpdateTitle [
	"Trying to have a clean subscription, this method is called in #initialize"
	
	self updateTitle.
	textModel text: textModel getText.
]

{ #category : #testing }
MessageBrowser >> cacheHierarchyForClasses: aCollection [

	cachedHierarchy := self buildHierarchyForMessages: aCollection.
]

{ #category : #announcements }
MessageBrowser >> classModified: anAnnouncement [
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer: [ 
		self handleClassModified: anAnnouncement ]
]

{ #category : #announcements }
MessageBrowser >> classRenamed: anAnnouncement [ 
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer: [ 
		self handleClassRenamed: anAnnouncement ]
]

{ #category : #private }
MessageBrowser >> codeFont [
	^ StandardFonts codeFont
]

{ #category : #accessing }
MessageBrowser >> currentMethod [
	^ self selectedMessage
]

{ #category : #api }
MessageBrowser >> displayBlock: aBlock [

	^ listModel displayBlock: aBlock
]

{ #category : #'private-focus' }
MessageBrowser >> ensureKeyBindingsFor: aWidget [
	
	self registerListShortcutsFor: listModel
]

{ #category : #'text selection' }
MessageBrowser >> findFirstOccurrenceOf: searchedString in: textToSearchIn [
	"Return the first index of aString in textToSearchIn "
	| firstIndex |
	firstIndex := textToSearchIn findString: searchedString startingAt: 1.
	[ (firstIndex > 1) and: [ (textToSearchIn at: (firstIndex - 1)) isAlphaNumeric ] ]
		whileTrue: [
			firstIndex := textToSearchIn findString: searchedString startingAt: firstIndex +1 ].
		
	^ firstIndex
]

{ #category : #announcements }
MessageBrowser >> handleClassRenamed: anAnnouncement [
	
	| items selectedIndex |
	items := listModel listItems collect: [ :rgMethod | | interestedClassName interestedClass |
		interestedClassName := anAnnouncement oldName.
		interestedClass := anAnnouncement classRenamed.
		rgMethod isMetaSide ifTrue: [ 
			interestedClassName := interestedClassName, ' class'.
			interestedClass := interestedClass theMetaClass ].
		rgMethod parentName = interestedClassName ifTrue: [ 
			(interestedClass >> rgMethod selector) asRing2Definition.
		 	] ifFalse: [ rgMethod ] ].
	
	selectedIndex := listModel selectedIndex.
	self messages: items.
	listModel setSelectedIndex: selectedIndex.
]

{ #category : #announcements }
MessageBrowser >> handleMethodAdded: anAnnouncement [
	| item sel text boolean |

	self isDisplayed ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	
	item := anAnnouncement method.
	
	(self shouldRefreshItem: item fromAnnouncement: anAnnouncement) ifFalse: [ ^ self ].
	((item methodClass notNil) and:[item methodClass isObsolete not]) ifFalse: [ ^ self ].
	
	boolean := textModel hasUnacceptedEdits.
	boolean ifTrue: [ text := textModel pendingText ].
	
	sel := listModel selectedItem.
	self messages: (listModel listItems add: item asRing2Definition; yourself).
	listModel setSelectedItem: sel.
	
	boolean ifTrue: [ textModel pendingText: text ]
]

{ #category : #announcements }
MessageBrowser >> handleMethodModified: anAnnouncement [
	| item oldItem sel index text list edits |
	self isDisplayed
		ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	item := anAnnouncement newMethod.
	oldItem := anAnnouncement oldMethod.
	sel := listModel selectedItem.
	sel ifNil: [ ^ self ].
	(sel methodClass = oldItem methodClass
		and: [ sel selector = oldItem selector ])
		ifFalse: [ ^ self ].
	(self shouldRefreshItem: item fromAnnouncement: anAnnouncement)
		ifFalse: [ ^ self ].
	edits := textModel hasUnacceptedEdits.
	edits
		ifTrue: [ text := textModel pendingText ].
	index := listModel selectedIndex.
	list := listModel listItems
		remove: sel ifAbsent: [  ];
		add: item asRing2Definition;
		"to ensure it's still a RGMethod"
			yourself.
	self messages: list.
	listModel setSelectedIndex: index.
	edits
		ifFalse: [ ^ self ].
	textModel pendingText: text.
	textModel hasEditingConflicts: true
]

{ #category : #announcements }
MessageBrowser >> handleMethodRemoved: anAnnouncement [
	| item sel itm |
	
	self isDisplayed ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	self okToChange ifFalse: [ ^ self ].
	
	item := anAnnouncement method.	"Item is a compiled methed, where the list is populated with RGMethod"
	
	((item methodClass notNil) and:[item methodClass isObsolete not]) ifFalse:[ ^ self ].
	
	sel := listModel selectedIndex.
	itm := listModel selectedItem.
	
	(itm notNil and: [ itm methodClass = item methodClass and: [ itm selector = item selector ] ])
		ifTrue: [ textModel hasUnacceptedEdits: false ].
	
	self
		messages:
			(listModel listItems
				remove: item asRing2Definition ifAbsent: [ nil ];
				yourself).
	
	listModel setSelectedIndex: sel
]

{ #category : #private }
MessageBrowser >> initialExtent [

	^ (700 min: (World extent x)) @ (550 min: (World extent y))
]

{ #category : #initialization }
MessageBrowser >> initialize [
	textConverterHolder := SourceMethodConverter new asValueHolder.
	model := AbstractTool new.
	topologicSortHolder := true asValueHolder.
	super initialize.
	askOkToClose := true asValueHolder.
	self windowIcon: self taskbarIcon.
	self registerToAnnouncements.
	self announcer when: WidgetBuilt send: #buildUpdateTitle to: self
]

{ #category : #initialization }
MessageBrowser >> initializeDropList [

	toolbarModel 
		addItemLabeled: 'Source' do: [ self textConverter: SourceMethodConverter new ];
		addItemLabeled: 'Time stamp' do: [ self textConverter: TimeStampMethodConverter new ]
]

{ #category : #initialization }
MessageBrowser >> initializePresenter [

	listModel whenSelectedItemChanged: [:item |
			toolbarModel method: item.
			textModel behavior: (item ifNil: [ nil ] ifNotNil: [ item methodClass ]).
			textModel doItReceiver: textModel behavior.
			textModel text: (self textConverter method: item; getText).
			textModel clearUndoManager.
			self installIconStylerFor: item].
	listModel whenListChanged: [ self updateTitle ].
		
	titleHolder whenChangedDo: [ self updateTitle ].
	textModel acceptBlock: [ :text :notifyer | (self accept: text notifying: notifyer)  notNil].
]

{ #category : #initialization }
MessageBrowser >> initializeWidgets [

	listModel := self instantiate: FastTablePresenter.
	textModel := self newCode.
	toolbarModel := self instantiate: MethodToolbar.
	
	listModel numberOfColumns: 3 withWidths: #(250 250 50).
	listModel displayBlock: [ :item | self wrapItem: item ].
	textModel aboutToStyle: true.
	textModel whenBuiltDo: [ :ann | ann widget font: self codeFont ].
	refreshingBlockHolder := [ :item | true ] asValueHolder.
				
	self
		setListMenu;
		initializeDropList;
		setFocus.

]

{ #category : #actions }
MessageBrowser >> inspectMethod [
	self currentMethod ifNotNil: [ :m | m inspect ]
]

{ #category : #private }
MessageBrowser >> installIconStylerFor: anItem [
	"icons styler are only supported for method definitions (not nil / not for comment definitions (those may occure
	for comment or general string searches))"

	(self isMethodDefinition: anItem)
		ifFalse: [ ^ self ].
		
	IconStyler styleText: self textModel withAst: anItem ast.
	anItem annotateRubricText: self textModel
]

{ #category : #private }
MessageBrowser >> intervalOf: aSelector inCode: aText of: aMethod [
	aMethod compiledMethod ifNil: [  ^ 0 to: 0 ].
	^ (self sourceIntervalOf: aSelector in: aMethod ast) ifEmpty: [ self searchedString: aSelector asString in: aText ]
]

{ #category : #private }
MessageBrowser >> intervalOf: aSelector inCommentText: aText [
	^ self searchedString: aSelector asString in: aText
]

{ #category : #private }
MessageBrowser >> isMethodDefinition: anItem [
	"an item can be nil, a compiled method, or a ring object. Whereas a ring object can be a ring method or comment definition.
	 Some operations are only valid for compiled methods or ring object that are a method definitions"

	anItem ifNil: [ ^ false ].
	
	"assume it is a compiled method or ring method definition"
	^ (anItem isRing2Object and: [ anItem isMethod and: [ anItem compiledMethod isNotNil ] ])
		or: [  anItem isCompiledMethod ]
]

{ #category : #accessing }
MessageBrowser >> listModel [
	
	^ listModel
]

{ #category : #api }
MessageBrowser >> messages [

	^ listModel listItems
]

{ #category : #api }
MessageBrowser >> messages: aCollection [

	self cacheHierarchyForClasses: aCollection.
	listModel items: (cachedHierarchy keys sort: [:a :b | self sortClassesInCachedHierarchy: a b: b]) asOrderedCollection.
	listModel listSize > 0
		ifTrue: [ 
			listModel setSelectedIndex: 1 ].
]

{ #category : #announcements }
MessageBrowser >> methodAdded: anAnnouncement [
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer: [ 
		self handleMethodAdded: anAnnouncement ]
]

{ #category : #private }
MessageBrowser >> methodClassNameForItem: anItem [
	^ anItem methodClass ifNotNil: [ :class | class name ] ifNil: [ '' ]
]

{ #category : #announcements }
MessageBrowser >> methodModified: anAnnouncement [
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer:  [	
		self handleMethodModified: anAnnouncement ]
]

{ #category : #announcements }
MessageBrowser >> methodRecategorized: aMethod [
]

{ #category : #announcements }
MessageBrowser >> methodRemoved: anAnnouncement [
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer:  [ 
		self handleMethodRemoved: anAnnouncement ]
]

{ #category : #accessing }
MessageBrowser >> model [ 
	^model
]

{ #category : #'messageList interface' }
MessageBrowser >> open [

	^self openWithSpec
]

{ #category : #private }
MessageBrowser >> packageNameForItem: anItem [
	^ anItem package ifNil: [ '' ] ifNotNil: [ :package | package name ]
]

{ #category : #private }
MessageBrowser >> protocolNameForItem: anItem [
	^ anItem category ifNil: [ '' ]
]

{ #category : #api }
MessageBrowser >> refreshingBlock: aBlock [

	refreshingBlockHolder value: aBlock
]

{ #category : #initialization }
MessageBrowser >> registerListShortcutsFor: aWidget [

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self browseMethod ].
	aWidget bindKeyCombination: PharoShortcuts current inspectItShortcut toAction: [ self inspectMethod ].
	aWidget bindKeyCombination: PharoShortcuts current browseImplementorsShortcut toAction: [ self browseMessages ].
	aWidget bindKeyCombination: PharoShortcuts current browseSendersShortcut toAction: [ self browseSendersOfMessage ].
	aWidget bindKeyCombination: PharoShortcuts current browseReferencesShortcut toAction: [ self browseClassRefs ].
	aWidget bindKeyCombination: PharoShortcuts current browseVersionsShortcut toAction: [ self browseVersions ].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self removeMethods ].
]

{ #category : #'announcements-registration' }
MessageBrowser >> registerToAnnouncements [

	SystemAnnouncer uniqueInstance weak		
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRecategorized send: #methodRecategorized: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: ClassRenamed send: #classRenamed: to: self
]

{ #category : #actions }
MessageBrowser >> removeMethods [
	self currentMethod ifNotNil: [ :method |
		model removeMethod: method inClass: method methodClass ]
]

{ #category : #api }
MessageBrowser >> resetSelection [
	
	listModel resetSelection
]

{ #category : #'text selection' }
MessageBrowser >> searchedString: searchedString in: aString [
	"Return the interval that corresponds to the portion of aString "
	"This method takes care of finding complete match to searchedString. "
	| string interval firstIndex |
	
	searchedString ifNil: [ ^ 0 to: 0 ].
	aString isEmptyOrNil ifTrue: [ ^0 to: 0 ].
	string := aString asString.
	interval := 0 to: 0.
	
	(searchedString includes: $:)
		ifTrue: [ | list |
			list := searchedString substrings: ':'.
			list size = 1
				ifTrue: [" binary selector "
					firstIndex := self findFirstOccurrenceOf: searchedString in: string.
					firstIndex isZero
						ifFalse: [ interval := firstIndex to: (firstIndex+searchedString size-1) ] ] 
					
				ifFalse: [
					| lastIndex |
					firstIndex := self findFirstOccurrenceOf: list first, ':' in: string. 
					firstIndex >0 ifTrue: [ 
								lastIndex := string findString: list last,':' startingAt: firstIndex+ (list first size -1).
								interval := firstIndex to: (lastIndex + list last size) ] ] ]
		ifFalse: [
			" unary selector "
			firstIndex := self findFirstOccurrenceOf: searchedString in: string.
			 firstIndex > 0
						ifTrue: [ interval := firstIndex to: (firstIndex+searchedString size - 1) ] ].
	^ interval
]

{ #category : #api }
MessageBrowser >> selectedClass [
	^ self selectedMethod origin
]

{ #category : #accessing }
MessageBrowser >> selectedItem [

	^ listModel selectedItem
]

{ #category : #api }
MessageBrowser >> selectedMessage [

	^ listModel selectedItem
]

{ #category : #api }
MessageBrowser >> selectedMessage: aMessage [

	listModel setSelectedItem: aMessage.
]

{ #category : #accessing }
MessageBrowser >> selectedMethod [
	self selectedMessage ifNil: [ ^nil ].
	^ self selectedMessage compiledMethod
]

{ #category : #accessing }
MessageBrowser >> selectedMethods [
	^ {self selectedMessage} asOrderedCollection 
]

{ #category : #initialization }
MessageBrowser >> setFocus [

	self focusOrder
		add: listModel;
		add: toolbarModel;
		add: textModel
]

{ #category : #menus }
MessageBrowser >> setListMenu [
	listModel menu: [ :menu |
		menu addAllFromPragma: 'messageBrowserListMenu' target: self ].
]

{ #category : #api }
MessageBrowser >> setRefreshingBlockForImplementorsOf: aSelector [

	self refreshingBlock: [:message | message selector = aSelector ].
]

{ #category : #api }
MessageBrowser >> setRefreshingBlockForSendersOf: aSelector [
	| specialIndex |
	specialIndex := Smalltalk specialSelectorIndexOrNil: aSelector.
	self refreshingBlock: [:method | 
		method hasSelector: aSelector specialSelectorIndex: specialIndex ]
]

{ #category : #api }
MessageBrowser >> setSelectedIndex: anIndex [

	listModel setSelectedIndex: anIndex 
]

{ #category : #announcements }
MessageBrowser >> shouldRefreshItem: item fromAnnouncement: anAnnouncement [
	
	"The refreshing block holder contains a block that accepts an item of the list, the received announcement and the message browser.
	It will anwer a boolean indicating if the modified item should be refreshed.
	
	Concretely, this is used to know if the contents of the selected message should be refreshed"
	^ refreshingBlockHolder value
		cull: item
		cull: anAnnouncement
		cull: self
]

{ #category : #testing }
MessageBrowser >> sortClassesInCachedHierarchy: aMethodDefinition b: otherMethodDefinition [
	"This method checks wether the cached inheritance hierarchy of a method should be before than the one of another method.
	It compares alphabetically the hierarchy using #compare:
	If both are alphabetically equals, it uses the size the hierarchy.
	We do not know why this is done like this."
	| aMethodHierarchy otherMethodHierarchy minSize|
	aMethodHierarchy := cachedHierarchy at: aMethodDefinition.
	otherMethodHierarchy := cachedHierarchy at: otherMethodDefinition.
	
	minSize := aMethodHierarchy size min: otherMethodHierarchy size.
	
	1 to: minSize do: [ :i | |compare|
		compare := (aMethodHierarchy at: i) printString compare: (otherMethodHierarchy at: i) printString.
		compare ~~ 2
			ifTrue: [  ^ compare == 1 ]].
	^  aMethodHierarchy size < otherMethodHierarchy size
]

{ #category : #api }
MessageBrowser >> sortingBlock: aBlock [

	listModel sortingBlock: aBlock
]

{ #category : #'text selection' }
MessageBrowser >> sourceIntervalOf: aSelector in: aMethodNode [
    | senderNode |
    senderNode := aMethodNode allChildren detect: [ :x | x isMessage and: [ x selector = aSelector ] ] ifNone: [ ^ #() ].
    ^ senderNode keywordsPositions first to: senderNode keywordsPositions last + senderNode keywords last size - 1 
]

{ #category : #accessing }
MessageBrowser >> textConverter [
	
	^ textConverterHolder value
]

{ #category : #accessing }
MessageBrowser >> textConverter: aTextConverter [

	textConverterHolder value: (aTextConverter method: self textConverter method).
	textModel aboutToStyle: self textConverter shouldShout .
	textModel text: self textConverter getText.
]

{ #category : #accessing }
MessageBrowser >> textModel [
	
	^ textModel
]

{ #category : #accessing }
MessageBrowser >> textModel: aModel [
	
	textModel := aModel
]

{ #category : #private }
MessageBrowser >> title [
		
	^ titleHolder value , ' [' , listModel listSize printString , ']'
]

{ #category : #accessing }
MessageBrowser >> toolbarModel [
	
	^ toolbarModel
]

{ #category : #api }
MessageBrowser >> topologicSort [
	
	^ topologicSortHolder value
]

{ #category : #api }
MessageBrowser >> topologicSort: aBoolean [
	
	^ topologicSortHolder value: aBoolean
]

{ #category : #'api-events' }
MessageBrowser >> whenSelectedItemChanged: aBlock [ 

	listModel whenSelectedItemChanged: aBlock 
]

{ #category : #private }
MessageBrowser >> windowIsClosing [
	
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #private }
MessageBrowser >> wrapItem: anItem [
	| s packageName |
	s := String
		streamContents: [ :aStream | 
			3 to: (cachedHierarchy at: anItem) size do: [ :i | aStream << '    ' ].
			aStream << (self methodClassNameForItem: anItem) << ' ('.
			anItem isFromTrait
				ifTrue: [ aStream
						<< anItem compiledMethod origin name;
						space ].
			aStream << (self protocolNameForItem: anItem) << ')' ].
	packageName := self packageNameForItem: anItem.
	^ {s. anItem selector. ('[' , packageName , ']')}
]
