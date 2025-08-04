"
I am a classic four panes Smalltalk browser.

The instance side and class side mode is based on #metaLevelScope class. It affects the query scope which is used to show methods and method groups.

The method groups and variables mode is based on #methodGroupQuery: 
- when user switches to variables the groups query is replaced by ClyAllVariables query based on same scope.
- when user switches to groups the variables query is replaced by ClyAllMethodGroups query based on same scope.

Also methodGroupQuery is used to keep current method visibility level:
- when user enables some superclass visibility it just adds to the current query scope.

In addition I implement special logic for extending classes and default visibility of traits:

- when extending class is selected (which is gray in class view) I switches to its one meta level.
- when extending class is selected I add at the top of list the extending package method group

Internal Representation and Key Implementation Points.

    Instance Variables
	packageView:		<ClyQueryView>
	classView:		<ClyQueryView>
	methodGroupView:		<ClyQueryView>
	methodView:		<ClyQueryView>
	metaLevelScope:		<ClyMetaLevelClassScope>
	methodGroupQuery:		<ClyQuery>
"
Class {
	#name : 'ClyFullBrowserMorph',
	#superclass : 'ClyBrowserMorph',
	#instVars : [
		'packageView',
		'classView',
		'methodGroupView',
		'methodView',
		'metaLevelScope',
		'methodGroupQuery'
	],
	#classVars : [
		'DefaultPackageFilter',
		'ShouldDisplayProjectView'
	],
	#category : 'Calypso-SystemTools-FullBrowser-UI',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'UI'
}

{ #category : 'tools registration' }
ClyFullBrowserMorph class >> beDefaultBrowser [
	<script>
	self registerToolsOn: Smalltalk tools
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> onDefaultEnvironment [
	<script>
	^ (self on: ClyNavigationEnvironment currentImage)
			prepareInitialState;
			yourself
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> open [
	<script>
	^ self openOn: ClyNavigationEnvironment currentImage
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOn: aNavigationEnvironment [

	| browser |
	browser := self on: aNavigationEnvironment.
	browser open.
	^browser
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOn: aNavigationEnvironment withState: navigationBlock [

	| browser |
	browser := self on: aNavigationEnvironment.
	browser prepareInitialStateBy: navigationBlock.
	browser open.
	^browser
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOnClass: aClass [

	^self openWithState: [:browser | browser selectClass: aClass]
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOnClass: aClass selector: aSymbol [

	^(aClass lookupSelector: aSymbol)
		ifNil: [ self openOnClass: aClass ]
		ifNotNil: [:method | self openOnMethod: method]
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOnClass: aClass selector: aSymbol highlight: aString [

	| browser |
	browser := self openOnClass: aClass selector: aSymbol.
	browser withTool: ClyMethodCodeEditorToolMorph do: [ :editor |
		editor selectAnyString: { aString }].
	^browser
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOnMethod: aMethod [

	^self openWithState: [:browser | browser selectMethod: aMethod]
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openOnPackage: aPackage [

	^self openWithState: [:browser | browser selectPackage: aPackage]
]

{ #category : 'opening' }
ClyFullBrowserMorph class >> openWithState: navigationBlock [

	^ self openOn: ClyNavigationEnvironment currentImage withState: navigationBlock
]

{ #category : 'tools registration' }
ClyFullBrowserMorph class >> registerToolsOn: registry [

	registry register: self as: #browser
]

{ #category : 'settings' }
ClyFullBrowserMorph class >> settingsGroupOn: aBuilder [

	<systemsettings>
	(aBuilder group: #Calypso)
		label: 'Calypso Browser';
		parent: #codeBrowsing;
		description: 'Settings related to the browser';
		with: [
			(aBuilder setting: #shouldDisplayProjectView)
				label: 'Should display project view by default';
				default: false;
				target: self;
				description:
					'If true, Calypso will open a a project view. A project view list all the baselines in the current image as a project. A project can be open to display the packages inside of it. If false, displays the list of all packages present in the image.' ]
]

{ #category : 'accessing' }
ClyFullBrowserMorph class >> shouldDisplayProjectView [

	^ ShouldDisplayProjectView ifNil: [ ShouldDisplayProjectView := false ]
]

{ #category : 'accessing' }
ClyFullBrowserMorph class >> shouldDisplayProjectView: anObject [

	ShouldDisplayProjectView := anObject
]

{ #category : 'icons' }
ClyFullBrowserMorph class >> taskbarIconName [
	^#smallSystemBrowser
]

{ #category : 'private' }
ClyFullBrowserMorph >> adoptForExtendedClass [

	| extendingPackages extraGroups |
	self classSelection isEmpty ifTrue: [ ^ self ].
	methodGroupQuery := methodGroupQuery withoutItemsOfType: Package. "To remove previous package injection"
	(self classSelection lastSelectedItem isMarkedWith: ClyExtendedClassTag) ifFalse: [ ^ self ].

	"it will ensures method groups mode for extended class
	to auto select extension methods of selected packages"
	methodGroupQuery := ClyAllMethodGroupsQuery sortedFrom: self classScopeForMethods.

	extendingPackages := self packageSelection
		                     ifEmpty: [ "In case we are not in the package view but the project view we collect the extensions packages in the project."
			                     | extendingPackagesOfClass |
			                     extendingPackagesOfClass := self classSelection lastSelectedItem actualObject extendingPackages.
			                     self projectSelection actualObjects flatCollect: [ :project |
				                     project packages select: [ :package | extendingPackagesOfClass includes: package ] ] ]
		                     ifNotEmpty: [ :selection | selection actualObjects ].
	extraGroups := ClyConstantQuery returning: extendingPackages as: ClyGroupedExtendingPackagesResult new.
	extraGroups := methodGroupQuery scope adoptQuery: extraGroups.
	methodGroupQuery := methodGroupQuery , extraGroups
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> allNavigationScopes [

	| classLocalScope |
	classLocalScope := self classSelection asItemsScope: ClyBothMetaLevelClassScope.

	^super allNavigationScopes, {
		self packageSelection asItemsScope: ClyPackageScope.
		classLocalScope asFullHierarchyScope.
		classLocalScope }, ScopesManager availableScopes
]

{ #category : 'updating' }
ClyFullBrowserMorph >> buildTitleFromSelections [

	| newTitle mainItem |
	self projectSelection isEmpty ifFalse: [ newTitle := 'Project: ' , self projectSelection lastSelectedItem name ].

	self packageSelection isEmpty ifFalse: [ newTitle := 'Package: ' , self packageSelection lastSelectedItem name ].

	self classSelection isEmpty ifTrue: [ ^ newTitle ].

	mainItem := self methodSelection isEmpty
		            ifTrue: [ self classSelection lastSelectedItem ]
		            ifFalse: [ self methodSelection lastSelectedItem ].

	newTitle := mainItem systemDefinition printDefiningClass asString.
	mainItem systemDefinition isInstanceSide & (metaLevelScope = ClyClassSideScope) ifTrue: [ newTitle := newTitle , ' class' ].
	self methodSelection isEmpty ifFalse: [ newTitle := newTitle , '>>' , self methodSelection lastSelectedItem name ].
	^ (self methodSelection isEmpty
		   ifTrue: [ 'Class: ' ]
		   ifFalse: [ 'Method: ' ]) , newTitle
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> changeMethodGroupQueryBy: aBlock [

	self changeStateBy: [
		methodGroupQuery := aBlock value.
		methodGroupView showQuery: methodGroupQuery.
		methodGroupView initiateUIChangeBy: [
			"view should know that any change here was not requested by user"
			self methodGroupSelectionChanged]
	]
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> chooseClassForEditorOfMethod: aMethod [

	| selectedClass |
	self classSelection isSingleSelected ifTrue: [
		selectedClass := self classSelection lastSelectedItem actualObject.
		^self currentMetaLevelOf: selectedClass ].

	^aMethod origin
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> chooseClassesForNewMethod [

	^self classSelection actualObjects collect: [ :each |
		metaLevelScope metaLevelOf: each]
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> chooseClassesForNewMethod: aMethod [

	| methodClass selectedClasses |
	methodClass := aMethod origin.

	selectedClasses := self chooseClassesForNewMethod.
	selectedClasses := selectedClasses copyWithout: methodClass.
	^selectedClasses, {methodClass}
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> classGroupSelection [
	^packageView selection asSelectedItemsOf: ClyClassGroup
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> classScopeForMethods [
	| currentClassScope |
	currentClassScope := methodGroupQuery scope asScope: metaLevelScope.

	^self extendClassScopeForMethods: currentClassScope
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> classSelection [
	^classView selection
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> classSelectionChanged [

	self changeStateBy: [
		self showsFlatClasses ifFalse: [
			self silentlySelectPackageOfSelectedClass].

		self changeMethodGroupQueryBy: [
			self adoptForExtendedClass.
			methodGroupQuery restrictedByScope: self defaultClassScopeForMethods].

		self selectCurrentExtensionMethodGroup
	]
]

{ #category : 'accessing - view' }
ClyFullBrowserMorph >> classView [
	^ classView
]

{ #category : 'class compilation' }
ClyFullBrowserMorph >> compileANewClassFrom: aString notifying: aController startingFrom: aClass [

	^ self system compileANewClassFrom: aString notifying: aController startingFrom: aClass
]

{ #category : 'accessing - context' }
ClyFullBrowserMorph >> createCommandContext [
	^ClyFullBrowserGlobalContext for: self
]

{ #category : 'accessing - context' }
ClyFullBrowserMorph >> currentMetaLevelOf: aClass [
	^metaLevelScope metaLevelOf: aClass
]

{ #category : 'tools support' }
ClyFullBrowserMorph >> decorateMethodEditor: aMethodEditor [

	methodView query decorateResultMethodEditor: aMethodEditor
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> defaultClassScopeForMethods [

	| selectedClassScope |
	selectedClassScope := self classSelection asItemsScope: metaLevelScope.

	^self extendClassScopeForMethods: selectedClassScope
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> defaultNavigationScope [

	^self isScopedModeEnabled
		ifFalse: [ super defaultNavigationScope ]
		ifTrue: [ packageView query scope]
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> defaultPackageFilter [
	^DefaultPackageFilter ifNil: [
		DefaultPackageFilter := ClyTableFilterFactory of: ClyItemNameFilter using: ClyRegexPattern new]
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> extendClassScopeForMethods: aClassScope [
	| resultScope |

	resultScope := aClassScope.
	(ClyMethodVisibilityLevel allAvailableFor: self classSelection items)
		ifNotEmpty: [ :levels |
			"It will be likely one, but who knows"
			resultScope := levels anyOne extendClassScope: resultScope ].

	^resultScope
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> initialize [
	super initialize.
	metaLevelScope := ClyInstanceSideScope
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> initializeNavigationViews [
	"Initialize all panes (navigation views) of the browser.
	 Regular browsers have four panes: package, class, protocol and method."

	packageView := self newPackageView.
	classView := self newClassView.
	methodGroupView := self newMethodGroupView.
	methodView := self newMethodView
]

{ #category : 'testing' }
ClyFullBrowserMorph >> isClassSelected: aClass [
	"It is the logic to return true when metaclass is selected but given aClass is its instance side.
	In future this complexity should be addressed with first class ClassBrowserItem"
	^self classSelection items anySatisfy: [ :each |
		each actualObject == aClass or: [
			each systemDefinition definingClass == aClass ]]
]

{ #category : 'testing' }
ClyFullBrowserMorph >> isPackagePartOfSelection: aPackage [

	(packageView selection isBasedOnItemType: Project) ifTrue: [
		self projectSelection actualObjects
			detect: [ :project | project includesPackage: aPackage ]
			ifFound: [ ^ true ] ].

	^ self isPackageSelected: aPackage
]

{ #category : 'testing' }
ClyFullBrowserMorph >> isPackageSelected: aPackage [

	^self packageSelection includesActualObject: aPackage
]

{ #category : 'testing' }
ClyFullBrowserMorph >> isScopedModeEnabled [
	^packageView showsItemsFromScope: ClyPackageScope
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> metaLevelScope [
	^metaLevelScope
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> metaLevelScope: aMetaLevelScopeClass [
	metaLevelScope := aMetaLevelScopeClass
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> methodGroupQuery [
	^methodGroupQuery
]

{ #category : 'accessing' }
ClyFullBrowserMorph >> methodGroupQuery: aQuery [
	methodGroupQuery := aQuery
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> methodGroupSelection [
	^methodGroupView selection
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> methodGroupSelectionChanged [

	| methodQueries |
	methodGroupView changesWasInitiatedByUser ifTrue: [
		tabManager skipCurrentDesiredSelection]. "It will force selecting of method creation tab"

	self methodGroupSelection isEmpty ifTrue: [ ^self showAllMethods].

	methodQueries := self methodGroupSelection actualObjects
		collect: [ :eachGroup | eachGroup methodQuery].
	methodView
		showQueries: methodQueries as: (ClySortedQueryResult using: ClySortMethodBySelectorFunction ascending)
]

{ #category : 'accessing - view' }
ClyFullBrowserMorph >> methodGroupView [
	^ methodGroupView
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> methodSelection [
	^methodView selection
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> methodSelectionChanged [
	| actualMethods |

	actualMethods := self methodSelection actualObjects.
	methodGroupView highlightItemsWhich: [ :eachGroup |
		actualMethods anySatisfy: [ :eachMethod |
			eachGroup actualObject includesMethod: eachMethod ] ]
]

{ #category : 'accessing - view' }
ClyFullBrowserMorph >> methodView [
	^ methodView
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> navigateSpawnedFullBrowser: aFullBrowser [
	self isScopedModeEnabled ifTrue: [
		aFullBrowser switchToPackageScopeOf: packageView query scope basisObjects].

	self methodSelection isEmpty ifFalse: [
		^aFullBrowser selectMethod: self methodSelection lastSelectedItem actualObject].

	self classSelection isEmpty ifFalse: [
		^aFullBrowser selectClass: self classSelection lastSelectedItem actualObject].

	self packageSelection isEmpty ifFalse: [
		^aFullBrowser selectPackage: self packageSelection lastSelectedItem actualObject]
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> newClassView [
	"Creates the class pane"

	^ self newNavigationView
		displayMainColumnBy: [ :cell :item |
			cell useFullIndentation.
			self specifyClassItemNameOn: cell label for: item browserItem];
		enableFilter;
		enableCollapsingOfPreexpandedItems;
		requestNavigationBy: #classSelectionChanged
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> newMethodGroupView [
	"Creates the 'method group' pane (formerly the 'protocol' pane)"

	^ self newNavigationView
		treeStructure: {
			ClyMethodGroup -> #prepareSubgroupsQueryFrom:in: };
		requestNavigationBy: #methodGroupSelectionChanged;
		neverRestoreSelection
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> newMethodView [
	"Creates the method pane"

	^ self newNavigationView
		displayMainColumnBy: [ :cell :item |
			self specifyMethodItemNameOn: cell label for: item browserItem];
		requestNavigationBy: #methodSelectionChanged
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> newPackageView [
	"Creates the package pane"
	| newPackageView |

	newPackageView := self newNavigationView
		treeStructure: 	{
			Project -> #prepareProjectItemsQueryFrom:in:.
			Package -> #prepareClassGroupQueryFrom:in: };
		enableFilterUsing: ClyRegexPattern new;
		requestNavigationBy: #packageSelectionChanged.
	newPackageView filterField announcer
		when: RubTextChanged
		send: #updateDefaultPackageFilter
		to: self.

	^ newPackageView
]

{ #category : 'updating' }
ClyFullBrowserMorph >> newWindowTitle [
	| newTitle |

	packageView selection isEmpty ifTrue: [ ^self systemScope description].

	newTitle := self buildTitleFromSelections.
	self systemScope isCurrentImage ifFalse: [
		newTitle := newTitle , ' in ', self systemScope description ].

	^newTitle
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> packageSelection [
	^packageView selection asSelectedParentsOf: Package
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> packageSelectionChanged [

	self switchToFlatClasses
]

{ #category : 'accessing - view' }
ClyFullBrowserMorph >> packageView [
	^ packageView
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> prepareDefaultState [
	super prepareDefaultState.

	DefaultPackageFilter ifNotNil: [
		packageView activateFilterWith: DefaultPackageFilter].

	self morphicUIManager defer: [packageView takeKeyboardFocus]
]

{ #category : 'initialization' }
ClyFullBrowserMorph >> prepareInitialState [

	methodGroupQuery := ClyAllMethodGroupsQuery sortedFrom: (metaLevelScope emptyIn: navigationEnvironment).

	self class shouldDisplayProjectView
		ifTrue: [ self switchToProjects ]
		ifFalse: [ self switchToPackages ]
]

{ #category : 'accessing - selection' }
ClyFullBrowserMorph >> projectSelection [
	^packageView selection asSelectedParentsOf: Project
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> restrictMethodVisibilityBy: aClassScope [

	self changeMethodGroupQueryBy: [
		methodGroupQuery restrictedByScope: aClassScope]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectClass: aClass [

	| foundItems classDefinition |
	self changeStateBy: [
		self switchToMetaLevelScope: aClass metaLevelScope.
		foundItems := classView findItemsWith: { aClass instanceSide }.
		foundItems ifNotEmpty: [ "we want ensure that selected package is the package of found class.
			If it is not then we will select required package"
			classDefinition := foundItems anyOne systemDefinition.
			(self isPackageSelected: classDefinition definingPackage) ifTrue: [ ^ self classSelection selectItems: foundItems ] ].

		aClass packageTag isRoot
			ifTrue: [ self selectPackage: aClass package ]
			ifFalse: [ self selectPackage: aClass package andTag: aClass packageTagName ].

		self packageSelection isEmpty ifTrue: [ ^ self ].

		self showsFullClassHierarchy ifTrue: [ self switchToFullClassHierarchyOf: aClass ].
		self classSelection selectItemsWith: { aClass instanceSide } ]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectCurrentExtensionMethodGroup [

	self classSelection ifEmpty: [ ^ self ].
	self classSelection isMultipleSelected ifTrue: [ ^ self ].

	(self classSelection lastSelectedItem isMarkedWith: ClyExtendedClassTag) ifFalse: [ ^ self ].

	self packageSelection
		ifEmpty: [
			self projectSelection ifNotEmpty: [ :projects |
				self changeStateBy: [
					| packages |
					packages := projects lastSelectedItem actualObject packages.
					self methodGroupSelection selectItemsWhere: [ :each |
						(self packageOrganizer packageMatchingExtensionName: each name)
							ifNil: [ false ]
							ifNotNil: [ :package | packages includes: package ] ] ] ] ]
		ifNotEmpty: [ :selection | self selectMethodGroupNamed: self packageSelection lastSelectedItem name ]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectMethod: aMethod [

	| foundItems |
	self changeStateBy: [
		foundItems := methodView findItemsWith: { aMethod }.
		foundItems ifNotEmpty: [ ^self methodSelection selectItems: foundItems ].

		self selectClass: aMethod origin.
		self classSelection isEmpty ifTrue: [ ^self ].
		self methodGroupSelection beEmpty.
		self methodSelection selectItemsWith: {aMethod}
	]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectMethodGroupNamed: tagName [

	self changeStateBy: [
		self methodGroupSelection selectItemsWhere: [ :each | each name = tagName ]
	]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectPackage: aPackage [

	self changeStateBy: [
		packageView selection selectItemsWith: { aPackage }.
		packageView selection isEmpty ifTrue: [
			self showInPackageView: aPackage.
			packageView selection selectItemsWith: { aPackage } ] ]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectPackage: aPackage andTag: tagName [

	| packageItem targetClassGroup foundPackages |
	self changeStateBy: [
		foundPackages := packageView findItemsWith: { aPackage }.
		foundPackages ifEmpty: [
			self showInPackageView: aPackage.
			foundPackages := packageView findItemsWith: { aPackage } ].
		packageItem := foundPackages first.
		packageItem expand.
		targetClassGroup := packageItem childrenItems detect: [ :each | each name = tagName ].

		packageView selection selectItems: { targetClassGroup } ]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> selectVariableNamed: varName [

	self selectMethodGroupNamed: varName
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> setUpScopedModeWith: aScope [

	(aScope isKindOf: ClyPackageScope) ifFalse: [ ^self ].

	self switchToPackageScopeOf: aScope basisObjects
]

{ #category : 'updating' }
ClyFullBrowserMorph >> setWindowTitle: aString in: aWindow [
	| emphasis |
	super setWindowTitle: aString in: aWindow.

	aWindow basicLabel ifNotNil: [:label |
		emphasis := metaLevelScope = ClyInstanceSideScope
			ifTrue: [ TextEmphasis normal ] ifFalse: [ TextEmphasis bold ].
		label emphasis: emphasis emphasisCode]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> showAllMethods [

	| methodQuery |

	methodQuery := ClyAllMethodsQuery sortedFrom: self classScopeForMethods.
	methodView showQuery: methodQuery
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> showInPackageView: aPackage [
	"If true we are in the project view and we need to find the package and expand it"

	(packageView showsItemsFromQuery: ClyAllProjectsQuery) ifTrue: [
		self projectSelection selectItemsWhere: [ :each | (each representsItemOfType: Project) and: [ each actualObject includesPackage: aPackage ] ].
		self projectSelection items first expand ].
	self packageSelection selectItemsWith: { aPackage }
]

{ #category : 'testing' }
ClyFullBrowserMorph >> showsFlatClasses [

	^ (self projectSelection isNotEmpty or: [ self packageSelection isNotEmpty ]) and: [ (classView showsItemsFromScope: ClyClassHierarchyScope) not ]
]

{ #category : 'testing' }
ClyFullBrowserMorph >> showsFullClassHierarchy [
	^classView showsItemsFromScope: ClySuperclassScope
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> silentlySelectPackageOfSelectedClass [

	| selectedClass |
	self classSelection isEmpty ifTrue: [ ^ self ].

	selectedClass := self classSelection lastSelectedItem actualObject.

	packageView ignoreNavigationDuring: [
		selectedClass packageTag isRoot
			ifTrue: [ self selectPackage: selectedClass package ]
			ifFalse: [ self selectPackage: selectedClass package andTag: selectedClass packageTagName ] ]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> snapshotState [

	^ClyFullBrowserState of: self
]

{ #category : 'private' }
ClyFullBrowserMorph >> specifyClassItemNameOn: nameMorph for: classItem [
	| definition |
	nameMorph contents: classItem name.

	definition := classItem systemDefinition.

	self packageSelection isMultipleSelected ifTrue: [
		nameMorph contents: (definition printFullNameOf: classItem)].
	(self isPackagePartOfSelection: definition definingPackage)
		ifFalse: [nameMorph color: self theme classExtensionColor]
]

{ #category : 'private' }
ClyFullBrowserMorph >> specifyMethodItemNameOn: nameMorph for: methodItem [
	| definition |
	nameMorph contents: methodItem name.

	definition := methodItem	systemDefinition.
	(self isPackagePartOfSelection: definition definingPackage) ifFalse: [
		nameMorph color: self theme classExtensionColor].

	self classSelection isMultipleSelected ifTrue: [
		^nameMorph contents: (definition printFullNameOf: methodItem)].

	(self isClassSelected: definition definingClass) ifTrue: [ ^self].

	nameMorph emphasis: TextEmphasis italic emphasisCode.
	nameMorph contents: (definition printFullNameOf: methodItem)
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchClassViewTo: aQueryResult inScope: aScope [
	| classes |
	classes := ClyAllClassesQuery from: aScope as: aQueryResult.

	classView showQuery: classes
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToFlatClasses [

	| classQueries |
	classQueries := OrderedCollection new.
	packageView selection groupItemsByTypeAndDo: [ :typeClass :items |
		classQueries addAll: (typeClass prepareClassQueriesFrom: items in: navigationEnvironment)].

	classView showQueries: classQueries as: ClyExtensionLastSortedClassResult hierarchical
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToFullClassHierarchy [

	| classScope |
	classScope := self classSelection asItemsScope: ClyInstanceSideScope.

	self
		switchClassViewTo: ClyHierarchicallySortedClassesResult new
		inScope: classScope asFullHierarchyScope
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToFullClassHierarchyOf: aClass [

	| classScope |
	classScope := ClyInstanceSideScope of: aClass in: navigationEnvironment.

	self
		switchClassViewTo: ClyHierarchicallySortedClassesResult new
		inScope: classScope asFullHierarchyScope
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToMetaLevelScope: aMetaLevelScopeClass [
	metaLevelScope = aMetaLevelScopeClass ifTrue: [ ^self ].

	self changeMethodGroupQueryBy: [
		metaLevelScope := aMetaLevelScopeClass.
		methodGroupQuery withMetaLevelScope: metaLevelScope
	]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToMethodGroups [

	self changeMethodGroupQueryBy: [
		ClyAllMethodGroupsQuery sortedFrom: self classScopeForMethods
	]
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToPackageScopeOf: packages [

	| query scope |
	scope := ClyPackageScope ofAll: packages in: navigationEnvironment.
	scope name: 'Scoped pckg'.
	query := ClyAllPackagesQuery sortedFrom: scope.

	packageView showQuery: query
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToPackages [

	packageView showQuery: (ClyAllPackagesQuery sortedFrom: self systemScope)
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToProjects [

	packageView showQuery: (ClyAllProjectsQuery sortedFrom: self systemScope)
]

{ #category : 'navigation' }
ClyFullBrowserMorph >> switchToVariables [
	| classScope |

	self changeMethodGroupQueryBy: [
		classScope := self classScopeForMethods.
		ClyAllVariablesQuery
			from: classScope, classScope asInheritedScope
			as: ClyGroupedInstanceVariablesResult withDefaultHierarchy
	]
]

{ #category : 'taskbar-required' }
ClyFullBrowserMorph >> taskbarLabel [

	| title |
	title := self newWindowTitle.
	^ title allButFirst: (title findString: ': ')
]

{ #category : 'private' }
ClyFullBrowserMorph >> updateDefaultPackageFilter [

	DefaultPackageFilter := packageView filterField getText ifEmpty: [ nil ]
]

{ #category : 'updating' }
ClyFullBrowserMorph >> updateMethodEditorAfterPackageRemoval [
	"A package was removed from the receiver, update the method editor accordingly"
	
	self tabManager tools
		detect: [ :aTool | aTool class = ClyClassCreationToolMorph ]
		ifFound: [ : methodEditor | methodEditor resetStateForRemoval ].
]
