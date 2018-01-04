"
I'm a new browser based on RPackage and Announcements with fancy goodies:

	- groups ( you can create groups with your favorite classes)
	- multi-selections
	- environments
	- iconic buttons
	- hierarchy
	- pragma based menus
	
Adding to context menus.

On the class-side of your object, create a method that:
	1. takes one argument (a PragmaMenuAndShortcutRegistration).
	2. Begins with the pragma for whichever menu you want to extend (see AbstractNautilusUI ""menu pragmas"" protocol)
	
	For example, to extend the menu for the method list pane:
		methodMenu: aBuilder
			<nautilusGlobalMethodMenu>
			
	To see existing examples in your image, browse senders of that pragma
"
Class {
	#name : #Nautilus,
	#superclass : #Object,
	#instVars : [
		'announcer',
		'browsedEnvironment',
		'browsingHistory',
		'plugins',
		'selectedCategory',
		'selectedClass',
		'selectedGroup',
		'selectedMethod',
		'selectedPackage',
		'showCategories',
		'showComment',
		'sortHierarchically',
		'showInstance',
		'showHierarchy',
		'ui',
		'methodClassifier'
	],
	#classVars : [
		'MethodClassifierClass',
		'PluginClasses',
		'SortHierarchically'
	],
	#classInstVars : [
		'groups',
		'methodGroup',
		'useOldStyleKeys',
		'showSourceCodeAnnotation',
		'inlineCritiques'
	],
	#category : #Nautilus
}

{ #category : #'instance creation' }
Nautilus class >> browsedEnvironment: anEnvironment [

	^ self new browsedEnvironment: anEnvironment
]

{ #category : #groups }
Nautilus class >> buildGroupManager [
	| holder |
	holder := GroupHolder new.
	(holder addAPackageGroupSilentlyNamed: 'Work' packages: OrderedCollection new)
		removable: true;
		isFillable: true.
	(holder addAPackageGroupSilentlyNamed: self configurationsGroupName packages: self configurationPackages)
		removable: false;
		isFillable: true.
	^ holder
]

{ #category : #groups }
Nautilus class >> buildMethodGroupManager [
	| holder |
	holder := GroupHolder new.
	(holder addAMethodGroupSilentlyNamed: self lastModifiedMethodsGroupName methods: self lastModifiedMethods)
		removable: false.
	^ holder
]

{ #category : #cleanup }
Nautilus class >> cleanUp [
	AbstractNautilusUI resetIconCaches.
	self resetGroups.

]

{ #category : #groups }
Nautilus class >> configurationPackages [
	^ (RPackageOrganizer default packages
		select: [ :package | (package name beginsWith: 'ConfigurationOf') or: [ package name beginsWith: 'BaselineOf' ] ]) asOrderedCollection
]

{ #category : #groups }
Nautilus class >> configurationsGroupName [
	^ 'Configurations'
]

{ #category : #private }
Nautilus class >> defaultBrowsedEnvironment [

	^ RBBrowserEnvironment new
]

{ #category : #private }
Nautilus class >> defaultMaxSize [

	^ NavigationHistory defaultMaxSize
]

{ #category : #accessing }
Nautilus class >> groupsManager [
	^ groups ifNil: [ groups := self buildGroupManager ]
]

{ #category : #icon }
Nautilus class >> icon [ 

	^ AbstractNautilusUI icon
]

{ #category : #'initialize-release' }
Nautilus class >> initialize [
	SessionManager default registerToolClassNamed: self name
]

{ #category : #settings }
Nautilus class >> inlineCritiques [
	^ inlineCritiques ifNil: [ inlineCritiques := false ]
]

{ #category : #settings }
Nautilus class >> inlineCritiques: aBoolean [
	inlineCritiques := aBoolean
]

{ #category : #testing }
Nautilus class >> isNoCategoryAndNoMethodSelected [
	
	^ self selectedCategory isNil and: [ self selectedMethod isNil ]
]

{ #category : #groups }
Nautilus class >> lastModifiedMethods [
	^ RecentMessageList uniqueInstance methodReferenceList asSet asOrderedCollection
]

{ #category : #groups }
Nautilus class >> lastModifiedMethodsGroupName [
	^ 'Last Modified Methods'
]

{ #category : #'world menu' }
Nautilus class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'System Browser')
		parent: #MostUsedTools;
		action: [ Smalltalk tools openClassBrowser ];
		order: 0.1;
		keyText: 'o, b';
		help: 'System browser to browse and edit code.';
		icon: Smalltalk tools browser taskbarIcon
]

{ #category : #accessing }
Nautilus class >> methodClassifierClass [

	^ MethodClassifierClass ifNil: [ MethodClassifierClass := MethodClassifier ]
]

{ #category : #accessing }
Nautilus class >> methodClassifierClass: aClass [

	 MethodClassifierClass := aClass
]

{ #category : #accessing }
Nautilus class >> methodGroupManager [
	^ methodGroup ifNil: [ methodGroup := self buildMethodGroupManager ]
]

{ #category : #settings }
Nautilus class >> nautilusSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #codeBrowsing)
		label: 'Code Browsing';
		description: 'All settings concerned with code browsing look''n feel';
		with:
				[ (aBuilder group: #Nautilus)
				target: self;
				label: 'Nautilus Browser';
				parent: #codeBrowsing;
				description: 'Settings related to the browser';
				with:
						[ (aBuilder setting: #sortHierarchically)
						label: 'Show hierarchy by default ?';
						description: 'If false, Nautilus will show a flat list of classes'.
					(aBuilder setting: #showSourceCodeAnnotation)
						label: 'Show source code annotation pane ?';
						description: 'Display a pane that shows current position in the source code, allows code formating etc. It affects only for the new Nautilus windows'.
						
					(aBuilder setting: #useOldStyleKeys)
						label: 'Use old style keys ?';
						description: 'If true, the browser will use the old style keys (This is highly non-recommended, you will lose a lot of cool access keys)'.
						
					(aBuilder setting: #inlineCritiques)
						label: 'Show critiques inline?';
						description: 'When turned on the quality critiques will be displayed inline in the code area' ] ]
]

{ #category : #opening }
Nautilus class >> open [
	^ self openInEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openInEnvironment: anEnvironment [

	| instance |
	instance := self new.
	instance 
		browsedEnvironment: anEnvironment;
		open.
	instance ui 
		useLastPackagePatternString;
		showHierarchy: false.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnCategory: aCategory ofClass: aClass [

	^ self openOnCategory: aCategory ofClass: aClass inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnCategory: aCategory ofClass: aClass inEnvironment: anEnvironment [
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance showInstance: aClass isMeta not.
	instance
		package: aClass package
		class: aClass
		protocol: aCategory.
	instance open.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass [

	^ self openOnClass: aClass inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass inEnvironment: anEnvironment [
	| instance |
	instance := self new.
	instance
		browsedEnvironment: anEnvironment;
		showInstance: aClass isMeta not;
		package: (self parentOfClass: aClass)
			class: aClass;
		open.
	instance ui
		useLastPackagePatternStringForClass: aClass;
		showHierarchy: false;
		giveFocusToProtocol.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass onGroup: aGroup [
	
	^ self openOnClass: aClass onGroup: aGroup inEnvironment: self defaultBrowsedEnvironment.
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass onGroup: aGroup inEnvironment: anEnvironment [
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance showInstance: aClass isMeta not.
	instance selectedGroup: aGroup.
	instance
		package: aClass package
		class: aClass.
	instance open.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass onProtocol: aProtocol [

	^ self 
		openOnClass: aClass 
		onProtocol: aProtocol 
		inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass onProtocol: aProtocol inEnvironment: anEnvironment [

	| instance |
	instance := self new.
	instance 
		browsedEnvironment: anEnvironment;
		showInstance: aClass isMeta not;
		selectedCategory: aProtocol;
		package: aClass package class: aClass;
		open.
	instance ui 
		showHierarchy: false.
	instance ui 
		selectedCategory: aProtocol;
		categorySelectionChanged;
		changed: #currentHistoryIndex.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnClass: aClass selector: aSelector [ 

	^ aSelector
		ifNil: [ self openOnClass: aClass ]
		ifNotNil: [| method |
			method := aClass methodDict at: aSelector ifAbsent: [ ^ self openOnClass:aClass ].
			method ifNotNil: [ self openOnMethod: method ]]
]

{ #category : #opening }
Nautilus class >> openOnClass: class selector: selector highlight: autoSelectString [
	| nautilus |

	nautilus := self openOnClass: class selector: selector.
	autoSelectString ifNotNil: [ nautilus ui highlight: autoSelectString ]
]

{ #category : #opening }
Nautilus class >> openOnGroup: aGroup [

	^ self openOnGroup: aGroup inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnGroup: aGroup inEnvironment: anEnvironment [
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance selectedGroup: aGroup.
	instance open.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnMethod: aMethod [
	
	^ self openOnMethod: aMethod inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnMethod: aMethod inEnvironment: anEnvironment [
	| instance class |
	class := aMethod methodClass.
	instance := self new.
	instance
		browsedEnvironment: anEnvironment;
		showInstance: class isMeta not;
		package: (self parentOfClass: class)
			class: class
			protocol: aMethod protocol asString
			method: aMethod;
		open.
	instance ui
		useLastPackagePatternStringForClass: class;
		showHierarchy: false;
		giveFocusToMethod.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnPackage: aPackage [

	^ self openOnPackage: aPackage inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnPackage: selectedPackage class: selectedClass category: selectedCategory method: selectedMethod group: selectedGroup sortHierarchically: sortHierarchically showHierarchy: showHierarchy showComment: showComment showInstance: showInstance showCategories: showCategories [
	^ self
		openOnPackage: selectedPackage
		class: selectedClass
		category: selectedCategory
		method: selectedMethod
		group: selectedGroup
		sortHierarchically: sortHierarchically
		showHierarchy: showHierarchy
		showComment: showComment
		showInstance: showInstance
		showCategories: showCategories
		inEnvironment: self defaultBrowsedEnvironment
]

{ #category : #opening }
Nautilus class >> openOnPackage: selectedPackage class: selectedClass category: selectedCategory method: selectedMethod group: selectedGroup sortHierarchically: sortHierarchically showHierarchy: showHierarchy showComment: showComment showInstance: showInstance showCategories: showCategories inEnvironment: anEnvironment [
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance
		package: selectedPackage
		class: selectedClass
		protocol: selectedCategory
		method: selectedMethod.
	instance selectedGroup: selectedGroup.
	instance
		sortHierarchically: sortHierarchically;
		showComment: showComment;
		showInstance: showInstance;
		showCategories: showCategories;
		showHierarchy: showHierarchy.
	instance open.
	instance ui
		showHierarchy: showHierarchy;
		showComment: showComment.
	^ instance
]

{ #category : #opening }
Nautilus class >> openOnPackage: aPackage inEnvironment: anEnvironment [

	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance selectedPackage: aPackage.
	instance open.
	instance ui showHierarchy: false.
	^ instance
]

{ #category : #accessing }
Nautilus class >> parentOfClass: aClass [
	"I can answer an RPackage or an RPackageTag, depending on the presence of Tags in package. 
	 This is an UI optimization to not show a tree RPackage->RPackageTag when there is just 
	 one tag in package, named in the same way as his parent"	

	| package tag |
	
	package := aClass package.
	tag := package classTagForClass: aClass.
	
	^ ((((tag name = package name) and: [ package classTags size <= 1 ]) or: [tag isNil])
		ifTrue: [ package ]
		ifFalse: [ tag ])
		asNautilusSelection
]

{ #category : #accessing }
Nautilus class >> pluginClasses [

	^ PluginClasses ifNil: [ PluginClasses := OrderedCollection new ].
]

{ #category : #accessing }
Nautilus class >> pluginClasses: aCollection [

	PluginClasses := aCollection
]

{ #category : #'tool - registration' }
Nautilus class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #browser

]

{ #category : #accessing }
Nautilus class >> resetGroups [
	<script>
	groups := nil
]

{ #category : #settings }
Nautilus class >> showSourceCodeAnnotation [
	^showSourceCodeAnnotation ifNil: [ showSourceCodeAnnotation := true ]
]

{ #category : #settings }
Nautilus class >> showSourceCodeAnnotation: aBoolean [
	showSourceCodeAnnotation := aBoolean
]

{ #category : #'system startup' }
Nautilus class >> shutDown [
	self cleanUp
]

{ #category : #accessing }
Nautilus class >> sortHierarchically [

	^ SortHierarchically ifNil: [ SortHierarchically := true ]
]

{ #category : #accessing }
Nautilus class >> sortHierarchically: aBoolean [

	SortHierarchically := aBoolean
]

{ #category : #events }
Nautilus class >> stopNotifications [
	" self stopNotifications "
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #icon }
Nautilus class >> taskbarIconName [

	^#nautilusIcon
]

{ #category : #settings }
Nautilus class >> useOldStyleKeys [
	^useOldStyleKeys ifNil: [ useOldStyleKeys := false ]
]

{ #category : #settings }
Nautilus class >> useOldStyleKeys: aBoolean [
	useOldStyleKeys := aBoolean
]

{ #category : #comparing }
Nautilus >> = aNautilus [

	self class = aNautilus class ifFalse: [ ^ false ].
	1 to: self class instVarNames size do: [:index |
		(self instVarAt: index) = (aNautilus instVarAt: index)
			ifFalse: [ ^ false ]].
	^ true
]

{ #category : #history }
Nautilus >> adopt: anAssociation [

	self browsingHistory pauseDuring: [
		anAssociation key applyTo: self ]
]

{ #category : #announcement }
Nautilus >> announce: anAnnouncement [

	self announcer announce: anAnnouncement

]

{ #category : #announcement }
Nautilus >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
Nautilus >> browsedEnvironment [

	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]
]

{ #category : #accessing }
Nautilus >> browsedEnvironment: anEnvironment [

	browsedEnvironment := anEnvironment
]

{ #category : #accessing }
Nautilus >> browsingHistory [

	^ browsingHistory ifNil: [ browsingHistory := NavigationHistory new ]
]

{ #category : #accessing }
Nautilus >> classes [
	
	^ self browsedEnvironment classes
]

{ #category : #accessing }
Nautilus >> classifier [
	^ methodClassifier ifNil: [ methodClassifier := self class methodClassifierClass new ]
]

{ #category : #accessing }
Nautilus >> classifier: aMethodClassifier [
	methodClassifier := aMethodClassifier
]

{ #category : #displaying }
Nautilus >> close [

	ui close
]

{ #category : #'browser compatibility' }
Nautilus >> codeTextMorph [

	^ self ui codeTextMorph
]

{ #category : #'browser compatibility' }
Nautilus >> contents: aText [
	self ui sourceCode: aText
]

{ #category : #private }
Nautilus >> foundExtentionsIn: aClass [

	^ aClass extendingPackages.

]

{ #category : #accessing }
Nautilus >> groupsManager [
	
	^ self class groupsManager
]

{ #category : #history }
Nautilus >> hasNext [
	
	^self browsingHistory hasNext
]

{ #category : #history }
Nautilus >> hasPrevious [
	
	^self browsingHistory hasPrevious
]

{ #category : #history }
Nautilus >> historyChanged [

	ui ifNotNil: [ ui historyChanged ]
]

{ #category : #history }
Nautilus >> historyEntries [

	^ self browsingHistory entries
]

{ #category : #initialization }
Nautilus >> initialize [
	super initialize.

	plugins := OrderedCollection new.
	self class pluginClasses do: [:each || class position |
		class := each first.
		position := each second.
		plugins add: ((class model: self) position: position; yourself)]
]

{ #category : #'browser compatibility' }
Nautilus >> labelString [
	^ self ui ifNil: [ 'Nautilus - System Browser' ] ifNotNil: [ :theui | theui title ]
]

{ #category : #history }
Nautilus >> next [
	self browsingHistory pauseDuring: [
		self adopt: self browsingHistory next.
		self historyChanged ].
]

{ #category : #displaying }
Nautilus >> open [
	ui := (NautilusUI on: self) open.
	self announce: (WindowOpened new window: ui window)
]

{ #category : #history }
Nautilus >> package: aPackage [

	self selectedPackage: aPackage.
	
	self announcer announce: (NautilusPackageSelected package: aPackage).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusPackageSelected ]
]

{ #category : #history }
Nautilus >> package: aPackage class: aClass [

	self 
		selectedPackage: aPackage;
		selectedClass: aClass.
		
	self announcer announce: (NautilusClassSelected package: aPackage class: aClass).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusClassSelected ].
	
	self registerHistoryNewEntry
]

{ #category : #history }
Nautilus >> package: aPackage class: aClass protocol: aProtocol [

	self 
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedCategory: aProtocol.
		
	self announcer announce: (NautilusProtocolSelected
										package: aPackage
										class: aClass
										protocol: aProtocol).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusProtocolSelected ].
	
	self registerHistoryNewEntry
]

{ #category : #history }
Nautilus >> package: aPackage class: aClass protocol: aProtocol method: aMethod [

	self 
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedCategory: aProtocol;
		selectedMethod: aMethod.
		
	self announcer announce: (NautilusMethodSelected
										package: aPackage
										class: aClass
										protocol: aProtocol
										method: aMethod).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusMethodSelected ].
	self ui ifNotNil: [ self ui resetIconStyle ].
	
	self registerHistoryNewEntry
]

{ #category : #'package filter' }
Nautilus >> packagePatternString: aString [ 
	
	self ui packagePatternString: aString
]

{ #category : #accessing }
Nautilus >> packages [

	^ self browsedEnvironment packages asArray
]

{ #category : #accessing }
Nautilus >> packagesWithoutExtensions [
	^ self browsedEnvironment packages asArray
]

{ #category : #accessing }
Nautilus >> parentOfClass: aClass [ 
	^ self class parentOfClass: aClass 
]

{ #category : #accessing }
Nautilus >> plugins [

	^ plugins ifNil: [ plugins := OrderedCollection new ]
]

{ #category : #history }
Nautilus >> previous [
	self browsingHistory pauseDuring: [
		self adopt: self browsingHistory previous.
		self historyChanged ]
]

{ #category : #history }
Nautilus >> registerHistoryNewEntry [
	| lastEntry wrapper |
	
	self browsingHistory isPaused ifTrue: [ ^ self ].
	wrapper := NautilusHistoryEntry with: self.
	self browsingHistory entries ifNotEmpty: [ :entries |
		lastEntry := entries last key.
		( lastEntry selectedClass isNil 
		or: [ ( wrapper selectedClass notNil 
			and: [ wrapper selectedMethod = lastEntry selectedMethod 
			and: [ wrapper selectedClass theNonMetaClass = lastEntry selectedClass theNonMetaClass ] ] ) ] ) 
				ifTrue: [ ^ self ] ].
	
	self browsingHistory add: wrapper.
	self historyChanged.
]

{ #category : #history }
Nautilus >> removeFromHistory: aNautilusHistoryEntry [

	self browsingHistory removeEntry: aNautilusHistoryEntry.
	self historyChanged
]

{ #category : #accessing }
Nautilus >> selectedCategory [

	^ selectedCategory
]

{ #category : #accessing }
Nautilus >> selectedCategory: anObject [
	selectedCategory := anObject.
	self selectedMethod: nil.
]

{ #category : #accessing }
Nautilus >> selectedClass [
	^ selectedClass
]

{ #category : #accessing }
Nautilus >> selectedClass: aClass [
	selectedClass := aClass.
	self selectedCategory: nil.
]

{ #category : #accessing }
Nautilus >> selectedGroup [

	^ selectedGroup
]

{ #category : #accessing }
Nautilus >> selectedGroup: anObject [
	selectedGroup := anObject.
	anObject
		ifNotNil: [ 
			self announcer announce: (NautilusGroupSelected group: anObject).
			self ui ifNotNil: [ self ui sourceTextModel announce: NautilusGroupSelected ] ]
]

{ #category : #accessing }
Nautilus >> selectedMethod [

	^ selectedMethod
]

{ #category : #accessing }
Nautilus >> selectedMethod: anObject [
	selectedMethod := anObject.
]

{ #category : #accessing }
Nautilus >> selectedPackage [

	^ selectedPackage
]

{ #category : #accessing }
Nautilus >> selectedPackage: anObject [
	selectedPackage := anObject asNautilusSelection.
	self selectedClass: nil
]

{ #category : #'browser compatibility' }
Nautilus >> setClass: aClass selector: aSelector [
	| method protocol |
	method := aClass methodDict at: aSelector ifAbsent: [ nil ].
	protocol := method ifNil: [ nil ] ifNotNil: [ method protocol ].
	self
		selectedPackage: (self parentOfClass: aClass);
		selectedClass: aClass;
		showInstance: aClass isMeta not;
		selectedCategory: protocol;
		selectedMethod: method
]

{ #category : #accessing }
Nautilus >> showCategories [

	^ showCategories ifNil: [ showCategories := true ]
]

{ #category : #accessing }
Nautilus >> showCategories: aBoolean [

	showCategories := aBoolean.
	self announcer announce: ( NautilusShowCategoriesChanged boolean: aBoolean )
]

{ #category : #accessing }
Nautilus >> showComment [

	^ showComment ifNil: [ showComment := false ]
]

{ #category : #accessing }
Nautilus >> showComment: aBoolean [

	showComment := aBoolean.
	self announcer announce: ( NautilusShowCommentChanged boolean: aBoolean )
]

{ #category : #accessing }
Nautilus >> showHierarchy [

	^ showHierarchy ifNil: [ showHierarchy := false ]
]

{ #category : #accessing }
Nautilus >> showHierarchy: aBoolean [

	showHierarchy := aBoolean.
	self announcer announce: ( NautilusShowPackagesChanged boolean: aBoolean not )
]

{ #category : #accessing }
Nautilus >> showInstance [

	^ showInstance ifNil: [ showInstance := true ]
]

{ #category : #accessing }
Nautilus >> showInstance: aBoolean [

	showInstance := aBoolean.
	self announcer announce: ( NautilusShowInstanceChanged boolean: aBoolean )
]

{ #category : #accessing }
Nautilus >> sortHierarchically [

	^ sortHierarchically ifNil: [ sortHierarchically := self class sortHierarchically ]
]

{ #category : #accessing }
Nautilus >> sortHierarchically: aBoolean [

	sortHierarchically := aBoolean.
	self announcer announce: ( NautilusShowHierarchyChanged boolean: aBoolean )
]

{ #category : #'browser compatibility' }
Nautilus >> spawnHierarchy [
	self
		sortHierarchically: true;
		open.
	self ui showHierarchy: true
]

{ #category : #'browser compatibility' }
Nautilus >> spawnHierarchyForClass: aClass selector: aSelector [
	self setClass: aClass selector: aSelector.
	self spawnHierarchy
]

{ #category : #accessing }
Nautilus >> ui [

	^ ui
]
