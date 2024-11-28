Class {
	#name : 'MCSpecSnapshotBrowser',
	#superclass : 'SpPresenter',
	#instVars : [
		'srcCodePresenter',
		'classesPresenter',
		'protocolsPresenter',
		'methodsPresenter',
		'items',
		'classOrInstanceSelectorPresenter',
		'categoriesPresenter',
		'commentPresenter',
		'classDefinitionPresenter',
		'titleString'
	],
	#category : 'MonticelloGUI-Tools',
	#package : 'MonticelloGUI',
	#tag : 'Tools'
}

{ #category : 'instance creation' }
MCSpecSnapshotBrowser class >> forSnapshot: aMCSnapshot [

	^ self on: aMCSnapshot
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> allClassNames [
	^ (items 
		select: [:ea | (ea isOrganizationDefinition | ea isScriptDefinition) not]
		thenCollect: [:ea | ea className]) asSet.

]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> application [

	^ StPharoApplication current
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> browserTitle [
		
	^ 'Snapshot Browser for: ' , titleString
]

{ #category : 'listing' }
MCSpecSnapshotBrowser >> categorySelection [

	^ categoriesPresenter selectedItem
]

{ #category : 'text' }
MCSpecSnapshotBrowser >> classCommentString [
	^ items
		detect: [ :ea | ea isClassDefinition and: [ ea className = self classSelection ] ]
		ifFound: [ :classDefinition | classDefinition comment ]
		ifNone: [ '' ]
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> classDefinitionString [

	| defs |
	defs := items select: [:ea | (ea isClassDefinition or: [ea isClassDefinitionExtension])
			and: [ea className = self classSelection]].

	defs isEmpty ifTrue: [^ 'This class is defined elsewhere.'].

	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | ea printDefinitionOn: stream]
			separatedBy: [stream nextPut: $.; cr] ].
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> classSelection [

	^ classesPresenter selectedItem
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> classTarget: aClass [

	^ self isClassSide
		  ifTrue: [ aClass class ]
		  ifFalse: [ aClass ]
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> connectPresenters [

	categoriesPresenter 
		transmitTo: classesPresenter
		transform: [ : aCategory | 
			aCategory
				ifNotNil: [ self visibleClasses ]
				ifNil: [	Array empty ] ].

	classesPresenter
		transmitTo: protocolsPresenter
		transform: [ : aClass | aClass
		  ifNotNil: [ self visibleProtocols ]
		  ifNil: [ Array empty ] ].
		
	classesPresenter
		transmitTo: classDefinitionPresenter 
		transform: [ : aClass | self classDefinitionString ].
		
	protocolsPresenter
		transmitTo: methodsPresenter
		transform: [ : aProtocolName | 
			aProtocolName
				ifNotNil: [ self visibleMethods ]
				ifNil: [ #(  ) ] ].
			
	methodsPresenter
		transmitTo: srcCodePresenter
		transform: [ :aMethod |
			aMethod
				ifNotNil: [ self updateSourceCode: aMethod ]
				ifNil: [ '' ] ].
]

{ #category : 'layout' }
MCSpecSnapshotBrowser >> defaultLayout [

	^ SpPanedLayout newTopToBottom
		  add: (SpBoxLayout newLeftToRight
				   spacing: self spacingBetweenPanes;
				   add: categoriesPresenter;
				   add: classesPresenter;
				   add: (SpBoxLayout newTopToBottom
						    add: protocolsPresenter;
						    add: classOrInstanceSelectorPresenter expand: false;
						    yourself);
				   add: methodsPresenter;
				   yourself);
		  add: (SpBoxLayout newLeftToRight
				   spacing: self spacingBetweenPanes;
				   add: (SpBoxLayout newTopToBottom
						    spacing: self spacingBetweenPanes;
						    add: classDefinitionPresenter;
						    add: commentPresenter;
						    yourself);
				   add: srcCodePresenter;
				   yourself);
		  yourself
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> extensionClassNames [
	^ (self allClassNames difference: self packageClassNames) asSortedCollection
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> extensionsCategory [
	^ '*Extensions'
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> hasExtensions [
	^ self extensionClassNames notEmpty
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeCategoriesPresenter [

	categoriesPresenter := self newFilteringList.
	categoriesPresenter
		items: self visibleCategories;
		headerTitle: 'Categories';   
		displayIcon: [ : aPackage | self iconNamed: aPackage systemIconName ];
		sortingBlock: [ :a :b | a < b ] ;
		contextMenu: [ (self rootCommandsGroup / 'RushPackageSelContextualMenu') beRoot asMenuPresenter ].

]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeClassDefinitionPresenter [

	classDefinitionPresenter := self newCode
		beNotEditable;
		yourself.
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeClassesPresenter [

	classesPresenter := self newFilteringList.
	classesPresenter
		items: self visibleClasses;
		headerTitle: 'Classes';
		displayIcon: [ :aClass | self iconNamed: aClass systemIconName ];
		sortingBlock: [ :a :b | a < b ];
		contextMenu: [ (self rootCommandsGroup / 'RushClassSelContextualMenu') beRoot asMenuPresenter ]
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeCodePresenter [

	srcCodePresenter := self newCode
		beNotEditable;
		yourself.
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeCommentPresenter [

	commentPresenter := self newText
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeMethodsPresenter [

	methodsPresenter := self newFilteringList.
	methodsPresenter
		items: OrderedCollection new;
		headerTitle: 'Methods';
		sortingBlock: [ :a :b | a < b ];
		contextMenu: [ (self rootCommandsGroup / 'RushMethodsSelContextualMenu') beRoot asMenuPresenter ]
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializePresenters [

	self initializeCategoriesPresenter.
	self initializeClassesPresenter.
	self initializeProtocolsPresenter.
	self initializeSideSelectorPresenter.
	self initializeClassDefinitionPresenter.
	self initializeMethodsPresenter.
	self initializeCodePresenter.
	self initializeCommentPresenter
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeProtocolsPresenter [

	protocolsPresenter := self newFilteringList.
	protocolsPresenter
		headerTitle: 'Protocols'
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeSideSelectorPresenter [

	classOrInstanceSelectorPresenter := self newCheckBox
		label: 'Class';
		whenActivatedDo: [ 
			protocolsPresenter 
				headerTitle: 'Class protocols';
				items: self visibleProtocols.
			classDefinitionPresenter text: self metaclassDefinitionString ];

		whenDeactivatedDo: [ 
			protocolsPresenter 
				headerTitle: 'Instance protocols';
				items: self visibleProtocols.
			classDefinitionPresenter text: self classDefinitionString  ];
		yourself.
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self browserTitle;
		initialExtent: 1000 @ 700;
		centered
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> isClassSide [
	"Answer <true> if the receiver's class side is selected"

	^ classOrInstanceSelectorPresenter isActive
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> metaclassDefinitionString [
	| defs |
	defs := items select: [:ea | (ea isClassDefinition or: [ea isClassDefinitionExtension])
			and: [ea className = self classSelection]].

	defs isEmpty ifTrue: [^ 'This class is defined elsewhere.'].

	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | ea printClassDefinitionOn: stream]
			separatedBy: [stream nextPut: $.; cr] ].
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> methodsForSelectedClass [

	^ items select: [ :ea | ea className = self classSelection and: [ ea isMethodDefinition and: [ ea classIsMeta = self switchIsClass ] ] ]
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> methodsForSelectedProtocol [

	| methods |
	
	self protocolSelection ifNil: [^ Array new].
	methods := self methodsForSelectedClass asOrderedCollection.
	(self protocolSelection = '-- all --') 
		ifFalse: [methods removeAllSuchThat: [:ea | ea protocol ~= self protocolSelection]].
	^ methods collect: #selector
	
								
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> packageClassNames [
	^ self packageClasses collect: [:ea | ea className]
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> packageClasses [
	^ items select: [:ea | ea isClassDefinition ]
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> packageOrganizations [

	^ items select: [:ea | ea isOrganizationDefinition]
]

{ #category : 'selecting' }
MCSpecSnapshotBrowser >> protocolSelection [
	
	^ protocolsPresenter selectedItem
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> selectedMethod [

	^ methodsPresenter selectedItem
]

{ #category : 'accessing - model' }
MCSpecSnapshotBrowser >> setModelBeforeInitialization: aMCSnapshot [

	items := aMCSnapshot definitions asSortedCollection.
	titleString := (aMCSnapshot definitions detect: #isOrganizationDefinition) packageName
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> spacingBetweenPanes [
	
	^ 5
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> switchIsClass [

	^ classOrInstanceSelectorPresenter state
]

{ #category : 'callbacks' }
MCSpecSnapshotBrowser >> updateSourceCode: aMCMethodSelector [ 

	| mcDef |
	mcDef := items detect: [ : mcObject |
		(mcObject isMethodDefinition and: [ mcObject className = self classSelection ])
			and: [ mcObject selector = aMCMethodSelector ] ].
	
	srcCodePresenter text: mcDef source.
	^  mcDef source
]

{ #category : 'accessing' }
MCSpecSnapshotBrowser >> visibleCategories [

	^ ((self packageOrganizations flatCollect: [ :ea | ea categories ]),
		(self packageClasses collect: [ :ea | ea category ]),
			(self hasExtensions ifTrue: [{ self extensionsCategory }] ifFalse: [#()]))
				asSet asSortedCollection
]

{ #category : 'listing' }
MCSpecSnapshotBrowser >> visibleClasses [

	^ self categorySelection = self extensionsCategory
		ifTrue: [ self extensionClassNames ]
		ifFalse: [
			self packageClasses
				select: [:ea | ea category = self categorySelection]
				thenCollect: [:ea | ea className ] ].
]

{ #category : 'listing' }
MCSpecSnapshotBrowser >> visibleMethods [

	^ self classSelection 
		ifNil: [#()]
		ifNotNil: [self methodsForSelectedProtocol]
]

{ #category : 'initialization' }
MCSpecSnapshotBrowser >> visibleProtocols [

	| methods |

	methods := self methodsForSelectedClass.
	commentPresenter text: self classCommentString.
	^ (methods collect: [ :ea | ea category ]) asSet asSortedCollection
]
