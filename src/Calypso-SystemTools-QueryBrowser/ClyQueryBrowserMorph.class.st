"
I am a query browser.
I show in one table result of any system query.

To show query use following method: 

	ClyQueryBrowser openOn: (ClyMessageSenders of: #do:)

I implement more simple API on class side: 

	ClyQueryBrowser browseSendersOf: #do:.
	ClyQueryBrowser browseImplementorsOf: #do:.
	ClyQueryBrowser browseMethods: {Point>>#x. Point>>#y}.

Last method is suitable to show given list of methods. But normally users should use first class queries.

I provide scoping mechanizm: user can filter query result using scopes from the compobox in toolbar.

When I spawned from the browser I receive all its navigation scopes.
For full browser I receive current selection package and class scopes.
For query browser I inherit all scopes which it has.
Also I add extra scopes to my scope list which is based on my current selection.
For example selected method will bring extra class and package scopes of this method.  

Internal Representation and Key Implementation Points.

    Instance Variables
	activeScope:		<ClyScope>
	queryScopes:		<OrderedCollection of<ClyScope>>
	resultView:		<ClyQueryView>
	systemQuery:		<ClyQuery>
"
Class {
	#name : #ClyQueryBrowserMorph,
	#superclass : #ClyBrowserMorph,
	#instVars : [
		'resultView',
		'systemQuery',
		'queryScopes',
		'activeScope',
		'expectedFlatQueryResult'
	],
	#classVars : [
		'DefaultQueryResult'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-UI'
}

{ #category : #opening }
ClyQueryBrowserMorph class >> browseClasses: classes [
	^self openOn: (ClyConstantQuery returning: classes)
]

{ #category : #opening }
ClyQueryBrowserMorph class >> browseImplementorsOf: aSymbol [
	^self openOn: (ClyMessageImplementorsQuery of: aSymbol)

]

{ #category : #opening }
ClyQueryBrowserMorph class >> browseMethods: methods [
	^self openOn: (ClyConstantMethodQuery with: methods)
]

{ #category : #opening }
ClyQueryBrowserMorph class >> browseMethods: methods withTitle: aString [
	^self openOn: (ClyConstantMethodQuery named: aString with: methods)
]

{ #category : #opening }
ClyQueryBrowserMorph class >> browseSendersOf: aSymbol [
	^self openOn: (ClyMessageSendersQuery of: aSymbol)
]

{ #category : #default }
ClyQueryBrowserMorph class >> defaultQueryResult [
	^DefaultQueryResult ifNil: [ 
		DefaultQueryResult := ClyHierarchicalSystemItemsResult new]
]

{ #category : #'instance creation' }
ClyQueryBrowserMorph class >> on: aNavigationEnvironment scopes: scopes [

	^(self on: aNavigationEnvironment) 
		queryScopes: scopes
]

{ #category : #opening }
ClyQueryBrowserMorph class >> openOn: aQuery [
	| browser |
	browser := self on: ClyNavigationEnvironment currentImage.
	browser showResultOf: aQuery.
	^browser open
]

{ #category : #icons }
ClyQueryBrowserMorph class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#referencesIcon
]

{ #category : #accessing }
ClyQueryBrowserMorph >> activeScope [
	^activeScope ifNil: [ activeScope := self systemScope ]
]

{ #category : #accessing }
ClyQueryBrowserMorph >> activeScope: anEnvironmentScope [
	activeScope := anEnvironmentScope
]

{ #category : #navigation }
ClyQueryBrowserMorph >> allNavigationScopes [
	| extraScopes |
	extraScopes := self extraScopesOfSelectedItems reject: #isBasedOnEmptyBasis.
	^queryScopes, (extraScopes copyWithoutAll: queryScopes)
]

{ #category : #accessing }
ClyQueryBrowserMorph >> chooseClassForEditorOfMethod: aMethod [
	^aMethod origin
]

{ #category : #accessing }
ClyQueryBrowserMorph >> chooseClassesForNewMethod: aMethod [
	^{aMethod origin}
]

{ #category : #initialization }
ClyQueryBrowserMorph >> classNameOf: aBrowserItem [

	^aBrowserItem systemDefinition printDefiningClass
]

{ #category : #accessing }
ClyQueryBrowserMorph >> classScopeOfSelectedItems [
	| classes |
	classes := self selection items
		collect: [ :methodItem | methodItem systemDefinition definingClass ].
	^ ClyBothMetaLevelClassScope ofAll: classes in: navigationEnvironment
]

{ #category : #'tools support' }
ClyQueryBrowserMorph >> decorateMethodEditor: aMethodEditor [

	systemQuery decorateResultMethodEditor: aMethodEditor
]

{ #category : #initialization }
ClyQueryBrowserMorph >> defaultFlatQueryResult [
	^ClySortedQueryResult using: ClySortSystemItemFunction ascending
]

{ #category : #navigation }
ClyQueryBrowserMorph >> defaultNavigationScope [
	^self activeScope
]

{ #category : #accessing }
ClyQueryBrowserMorph >> expectedFlatQueryResult [
	^ expectedFlatQueryResult
]

{ #category : #accessing }
ClyQueryBrowserMorph >> expectedFlatQueryResult: anObject [
	expectedFlatQueryResult := anObject
]

{ #category : #accessing }
ClyQueryBrowserMorph >> extraScopesOfSelectedItems [
	| classScope |
	classScope := self classScopeOfSelectedItems.
	^{
		self packageScopeOfSelectedItems. 
		classScope asFullHierarchyScope.
		classScope
	}
]

{ #category : #initialization }
ClyQueryBrowserMorph >> initialize [
	super initialize.
	
	queryScopes := OrderedCollection new.
	expectedFlatQueryResult := self defaultFlatQueryResult
]

{ #category : #initialization }
ClyQueryBrowserMorph >> initializeNavigationViews [
	resultView := self newNavigationView.
	resultView enableFilter: ClyQueryBrowserFilter.
	resultView allowsDeselection: false.
	resultView mainColumn 
		width: 250;
		displayItemPropertyBy: [:item | self classNameOf: item].
	(resultView addColumn: #name) 
		width: 200;
		displayItemPropertyBy: [:item | self mainNameOf: item].
	(resultView addColumn: #protocol) 
		width: 200;
		displayItemPropertyBy: [:item | self protocolNameOf: item].
	(resultView addColumn: #package) 
		width: 50;
		displayItemPropertyBy: [:item | self packageNameOf: item]
]

{ #category : #testing }
ClyQueryBrowserMorph >> isClassSelected: aClass [

	^self selection items anySatisfy: [ :methodItem |
		methodItem systemDefinition definingClass == aClass ]
]

{ #category : #testing }
ClyQueryBrowserMorph >> isQueryScopeActive: aScope [

	^self activeScope = aScope 
]

{ #category : #accessing }
ClyQueryBrowserMorph >> itemCount [
	^resultView itemCount
]

{ #category : #initialization }
ClyQueryBrowserMorph >> mainNameOf: aBrowserItem [
	aBrowserItem type = ClyClass ifTrue: [ ^'' ].
	
	^aBrowserItem name
]

{ #category : #accessing }
ClyQueryBrowserMorph >> methodSelection [
	^self selection asSelectedItemsOf: ClyMethod
]

{ #category : #navigation }
ClyQueryBrowserMorph >> navigateSpawnedFullBrowser: aFullBrowser [

	| lastItem definition |
	aFullBrowser setUpScopedModeWith: self activeScope.
	
	lastItem := self selection lastSelectedItem.
	definition := lastItem systemDefinition.
	
	aFullBrowser selectPackage: definition definingPackage.
	aFullBrowser selectClass: definition definingClass.
	
	(definition isKindOf: ClyMethodDefinitionProperty) ifTrue: [ 
		aFullBrowser selectMethod: lastItem actualObject].
]

{ #category : #updating }
ClyQueryBrowserMorph >> newWindowTitle [
	| title | 
	title := systemQuery description.
	resultView areItemsLoaded 
		ifTrue: [title := title capitalized , ' [' ,  self itemCount asString, ']']
		ifFalse: [title := 'Loading: ', title ].
	
	self systemScope isCurrentImage ifFalse: [ 
		title := title , ' in ', self systemScope description ].
	
	^title
]

{ #category : #'opening/closing' }
ClyQueryBrowserMorph >> openAnotherBrowser: aBrowser [
	(aBrowser isKindOf: ClyQueryBrowserMorph)
		ifTrue: [ aBrowser openInWindow: self window]
		ifFalse: [ aBrowser open ]
]

{ #category : #initialization }
ClyQueryBrowserMorph >> packageNameOf: aBrowserItem [

	^aBrowserItem systemDefinition definingPackageItem name
]

{ #category : #accessing }
ClyQueryBrowserMorph >> packageScopeOfSelectedItems [
	| packages |
	packages := self selection items
		collect: [ :methodItem | methodItem systemDefinition definingPackage ].
	^ ClyPackageScope ofAll: packages in: navigationEnvironment
]

{ #category : #initialization }
ClyQueryBrowserMorph >> prepareInitialState [
	"no initialization is required.
	If user do not set up system query I will be just with empty list"
]

{ #category : #initialization }
ClyQueryBrowserMorph >> protocolNameOf: aClyBrowserItem [ 
	^ aClyBrowserItem systemDefinition printDefiningProtocol
]

{ #category : #accessing }
ClyQueryBrowserMorph >> queryScopes [
	^queryScopes
]

{ #category : #accessing }
ClyQueryBrowserMorph >> queryScopes: scopesCollection [
	queryScopes := (scopesCollection
		reject: [ :each | each isBasedOnEmptyBasis])
		as: OrderedCollection 
]

{ #category : #updating }
ClyQueryBrowserMorph >> rebuildToolsForChangedEnvironment [
	super rebuildToolsForChangedEnvironment.
	
	tabManager tools ifEmpty: [ 
		resultView ensureSelectedItem.
		self rebuildAllTools ].
	self updateWindowTitle
]

{ #category : #accessing }
ClyQueryBrowserMorph >> requiredQueryResult [
	^systemQuery 
		ifNil: [self class defaultQueryResult]
		ifNotNil: [ systemQuery requiredResult  ]
]

{ #category : #accessing }
ClyQueryBrowserMorph >> resultView [
	^ resultView
]

{ #category : #navigation }
ClyQueryBrowserMorph >> selectClass: aClass [ 

	self selectObject: aClass
]

{ #category : #navigation }
ClyQueryBrowserMorph >> selectLastItem [
	
	self changeStateBy: [
		resultView selectLastItem
	]
]

{ #category : #navigation }
ClyQueryBrowserMorph >> selectMethod: aMethod [ 

	self selectObject: aMethod
]

{ #category : #navigation }
ClyQueryBrowserMorph >> selectObject: anObject [ 
	| foundItems |
	self changeStateBy: [
		foundItems := resultView findItemsWith: { anObject }.
	
		foundItems ifNotEmpty: [ 
			self selection selectItems: foundItems ]
	]
]

{ #category : #accessing }
ClyQueryBrowserMorph >> selection [
	^resultView selection
]

{ #category : #initialization }
ClyQueryBrowserMorph >> setUpActiveScope [
	systemQuery isBoundToEnvironment ifFalse: [ 
		systemQuery := systemQuery withScope: self systemScope].
	(queryScopes includes: systemQuery scope) ifFalse: [ 
		queryScopes add: systemQuery scope ].	
	activeScope := systemQuery scope
]

{ #category : #initialization }
ClyQueryBrowserMorph >> showQueryResult [	
	self showsFlatResult ifTrue: [ 
		"Browser keeps flat query result in variable 
		to be able switch back to it later from hierarchy mode"	
		expectedFlatQueryResult := systemQuery requiredResult ].
	
	self setUpActiveScope.
	resultView showQuery: systemQuery semiAsync
]

{ #category : #navigation }
ClyQueryBrowserMorph >> showResultOf: aQuery [
	"If given query has specific result then browser will show it as requested.
	Otherwise it will force query to build currently configured required result 
	(when given query has no explicitly specified result which means it has only default value)"		
	self changeStateBy: [
		systemQuery := aQuery buildsDefaultResult 
			ifTrue: [aQuery withResult: self requiredQueryResult]
			ifFalse: [ aQuery ].
		self showQueryResult
	]
]

{ #category : #testing }
ClyQueryBrowserMorph >> showsFlatResult [

	^(systemQuery retrievesItemsAs: ClyHierarchicalSystemItemsResult) not
]

{ #category : #testing }
ClyQueryBrowserMorph >> showsQueryResultAs: aQueryResult [

	^systemQuery requiredResult = aQueryResult
]

{ #category : #navigation }
ClyQueryBrowserMorph >> snapshotState [
	
	^ClyQueryBrowserState of: self
]

{ #category : #navigation }
ClyQueryBrowserMorph >> spawnFullBrowser [
	super spawnFullBrowser.
	
	self itemCount = 1 ifTrue: [ 
		[self close] on: ProvideAnswerNotification do: [:notification | 
			"If we have ditry tabs we are not able close it"
			notification resume: false]
	].
]

{ #category : #navigation }
ClyQueryBrowserMorph >> spawnQueryBrowserOn: aQuery withState: navigationState [
	
	| actualQuery |
	actualQuery := aQuery isBoundToEnvironment 
		ifTrue: [ aQuery ]
		ifFalse: [ aQuery withScope: activeScope].
	
	super spawnQueryBrowserOn: actualQuery withState: navigationState
]

{ #category : #navigation }
ClyQueryBrowserMorph >> switchResultTo: aQueryResult [

	self changeStateBy: [
		systemQuery := systemQuery withResult: aQueryResult.
		self showQueryResult
	]
]

{ #category : #navigation }
ClyQueryBrowserMorph >> switchScopeTo: aScope [
	
	self changeStateBy: [
		systemQuery := systemQuery withScope: aScope.
		self showQueryResult]
]

{ #category : #navigation }
ClyQueryBrowserMorph >> switchToFlatResult [
	"There is only type of default flat query result.
	If user passes query with specific flat result it should not modify default one.
	So senders and implementors will continue use same type of flat result.
	But when browser opens on query with special result it will use it localy to switch result mode"
	DefaultQueryResult := self defaultFlatQueryResult.
	self switchResultTo: expectedFlatQueryResult
]

{ #category : #navigation }
ClyQueryBrowserMorph >> switchToHierarchicalResult [

	DefaultQueryResult := ClyHierarchicalSystemItemsResult new.
	self switchResultTo: DefaultQueryResult
]

{ #category : #accessing }
ClyQueryBrowserMorph >> systemQuery [
	^ systemQuery
]

{ #category : #accessing }
ClyQueryBrowserMorph >> systemQuery: anObject [
	systemQuery := anObject
]
