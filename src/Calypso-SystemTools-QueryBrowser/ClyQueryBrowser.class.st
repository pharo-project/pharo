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
	#name : #ClyQueryBrowser,
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
ClyQueryBrowser class >> browseClasses: classes [
	^self openOn: (ClyConstantQuery returning: classes)
]

{ #category : #opening }
ClyQueryBrowser class >> browseImplementorsOf: aSymbol [
	^self openOn: (ClyMessageImplementorsQuery of: aSymbol)

]

{ #category : #opening }
ClyQueryBrowser class >> browseMethods: methods [
	^self openOn: (ClyConstantMethodQuery with: methods)
]

{ #category : #opening }
ClyQueryBrowser class >> browseMethods: methods withTitle: aString [
	^self openOn: (ClyConstantMethodQuery named: aString with: methods)
]

{ #category : #opening }
ClyQueryBrowser class >> browseSendersOf: aSymbol [
	^self openOn: (ClyMessageSendersQuery of: aSymbol)
]

{ #category : #default }
ClyQueryBrowser class >> defaultQueryResult [
	^DefaultQueryResult ifNil: [ 
		DefaultQueryResult := ClyHierarchicalSystemItems new]
]

{ #category : #'instance creation' }
ClyQueryBrowser class >> on: aNavigationEnvironment scopes: scopes [

	^(self on: aNavigationEnvironment) 
		queryScopes: scopes
]

{ #category : #opening }
ClyQueryBrowser class >> openOn: aQuery [
	| browser |
	browser := self on: ClyNavigationEnvironment currentImage.
	browser showResultOf: aQuery.
	^browser open
]

{ #category : #icons }
ClyQueryBrowser class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#referencesIcon
]

{ #category : #accessing }
ClyQueryBrowser >> activeScope [
	^activeScope ifNil: [ activeScope := self systemScope ]
]

{ #category : #accessing }
ClyQueryBrowser >> activeScope: anEnvironmentScope [
	activeScope := anEnvironmentScope
]

{ #category : #navigation }
ClyQueryBrowser >> allNavigationScopes [
	| extraScopes |
	extraScopes := self extraScopesOfSelectedItems reject: #isBasedOnEmptyBasis.
	^queryScopes, (extraScopes copyWithoutAll: queryScopes)
]

{ #category : #accessing }
ClyQueryBrowser >> chooseClassForEditorOfMethod: aMethod [
	^aMethod origin
]

{ #category : #accessing }
ClyQueryBrowser >> chooseClassesForNewMethod: aMethod [
	^{aMethod origin}
]

{ #category : #initialization }
ClyQueryBrowser >> classNameOf: aBrowserItem [

	^aBrowserItem systemDefinition printDefiningClass
]

{ #category : #accessing }
ClyQueryBrowser >> classScopeOfSelectedItems [
	| classes |
	classes := self selection items
		collect: [ :methodItem | methodItem systemDefinition definingClass ].
	^ ClyBothMetaLevelClassScope ofAll: classes in: navigationEnvironment
]

{ #category : #'tools support' }
ClyQueryBrowser >> decorateMethodEditor: aMethodEditor [

	systemQuery decorateResultMethodEditor: aMethodEditor
]

{ #category : #initialization }
ClyQueryBrowser >> defaultFlatQueryResult [
	^ClySortedQueryResult using: ClySortSystemItemFunction ascending
]

{ #category : #navigation }
ClyQueryBrowser >> defaultNavigationScope [
	^self activeScope
]

{ #category : #accessing }
ClyQueryBrowser >> expectedFlatQueryResult [
	^ expectedFlatQueryResult
]

{ #category : #accessing }
ClyQueryBrowser >> expectedFlatQueryResult: anObject [
	expectedFlatQueryResult := anObject
]

{ #category : #accessing }
ClyQueryBrowser >> extraScopesOfSelectedItems [
	| classScope |
	classScope := self classScopeOfSelectedItems.
	^{
		self packageScopeOfSelectedItems. 
		classScope asFullHierarchyScope.
		classScope
	}
]

{ #category : #initialization }
ClyQueryBrowser >> initialize [
	super initialize.
	
	queryScopes := OrderedCollection new.
	expectedFlatQueryResult := self defaultFlatQueryResult
]

{ #category : #initialization }
ClyQueryBrowser >> initializeNavigationViews [
	resultView := self newNavigationView.
	resultView enableFilter: ClyQueryBrowserFilter.
	resultView allowsDeselection: false.
	resultView mainColumn 
		width: 250;
		displayItemPropertyBy: [:item | self classNameOf: item].
	(resultView addColumn: #name) 
		width: 400;
		displayItemPropertyBy: [:item | self mainNameOf: item].
	(resultView addColumn: #package) 
		width: 50;
		displayItemPropertyBy: [:item | self packageNameOf: item]
]

{ #category : #testing }
ClyQueryBrowser >> isClassSelected: aClass [

	^self selection items anySatisfy: [ :methodItem |
		methodItem systemDefinition definingClass == aClass ]
]

{ #category : #testing }
ClyQueryBrowser >> isQueryScopeActive: aScope [

	^self activeScope = aScope 
]

{ #category : #accessing }
ClyQueryBrowser >> itemCount [
	^resultView itemCount
]

{ #category : #initialization }
ClyQueryBrowser >> mainNameOf: aBrowserItem [
	aBrowserItem type = ClyClass ifTrue: [ ^'' ].
	
	^aBrowserItem name
]

{ #category : #accessing }
ClyQueryBrowser >> methodSelection [
	^self selection asSelectedItemsOf: ClyMethod
]

{ #category : #navigation }
ClyQueryBrowser >> navigateSpawnedFullBrowser: aFullBrowser [

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
ClyQueryBrowser >> newWindowTitle [
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
ClyQueryBrowser >> openAnotherBrowser: aBrowser [
	(aBrowser isKindOf: ClyQueryBrowser)
		ifTrue: [ aBrowser openInWindow: self window]
		ifFalse: [ aBrowser open ]
]

{ #category : #initialization }
ClyQueryBrowser >> packageNameOf: aBrowserItem [

	^aBrowserItem systemDefinition definingPackageItem name
]

{ #category : #accessing }
ClyQueryBrowser >> packageScopeOfSelectedItems [
	| packages |
	packages := self selection items
		collect: [ :methodItem | methodItem systemDefinition definingPackage ].
	^ ClyPackageScope ofAll: packages in: navigationEnvironment
]

{ #category : #initialization }
ClyQueryBrowser >> prepareInitialState [
	"no initialization is required.
	If user do not set up system query I will be just with empty list"
]

{ #category : #accessing }
ClyQueryBrowser >> queryScopes [
	^queryScopes
]

{ #category : #accessing }
ClyQueryBrowser >> queryScopes: scopesCollection [
	queryScopes := (scopesCollection
		reject: [ :each | each isBasedOnEmptyBasis])
		as: OrderedCollection 
]

{ #category : #updating }
ClyQueryBrowser >> rebuildToolsForChangedEnvironment [
	super rebuildToolsForChangedEnvironment.
	
	tabManager tools ifEmpty: [ 
		resultView ensureSelectedItem.
		self rebuildAllTools ].
	self updateWindowTitle
]

{ #category : #accessing }
ClyQueryBrowser >> requiredQueryResult [
	^systemQuery 
		ifNil: [self class defaultQueryResult]
		ifNotNil: [ systemQuery requiredResult  ]
]

{ #category : #accessing }
ClyQueryBrowser >> resultView [
	^ resultView
]

{ #category : #navigation }
ClyQueryBrowser >> selectClass: aClass [ 

	self selectObject: aClass
]

{ #category : #navigation }
ClyQueryBrowser >> selectLastItem [
	
	self changeStateBy: [
		resultView selectLastItem
	]
]

{ #category : #navigation }
ClyQueryBrowser >> selectMethod: aMethod [ 

	self selectObject: aMethod
]

{ #category : #navigation }
ClyQueryBrowser >> selectObject: anObject [ 
	| foundItems |
	self changeStateBy: [
		foundItems := resultView findItemsWith: { anObject }.
	
		foundItems ifNotEmpty: [ 
			self selection selectItems: foundItems ]
	]
]

{ #category : #accessing }
ClyQueryBrowser >> selection [
	^resultView selection
]

{ #category : #initialization }
ClyQueryBrowser >> setUpActiveScope [
	systemQuery isBoundToEnvironment ifFalse: [ 
		systemQuery := systemQuery withScope: self systemScope].
	(queryScopes includes: systemQuery scope) ifFalse: [ 
		queryScopes add: systemQuery scope ].	
	activeScope := systemQuery scope
]

{ #category : #initialization }
ClyQueryBrowser >> showQueryResult [	
	self showsFlatResult ifTrue: [ 
		"Browser keeps flat query result in variable 
		to be able switch back to it later from hierarchy mode"	
		expectedFlatQueryResult := systemQuery requiredResult ].
	
	self setUpActiveScope.
	resultView showQuery: systemQuery semiAsync
]

{ #category : #navigation }
ClyQueryBrowser >> showResultOf: aQuery [
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
ClyQueryBrowser >> showsFlatResult [

	^(systemQuery retrievesItemsAs: ClyHierarchicalSystemItems) not
]

{ #category : #testing }
ClyQueryBrowser >> showsQueryResultAs: aQueryResult [

	^systemQuery requiredResult = aQueryResult
]

{ #category : #navigation }
ClyQueryBrowser >> snapshotState [
	
	^ClyQueryBrowserState of: self
]

{ #category : #navigation }
ClyQueryBrowser >> spawnFullBrowser [
	super spawnFullBrowser.
	
	self itemCount = 1 ifTrue: [ 
		[self close] on: ProvideAnswerNotification do: [:notification | 
			"If we have ditry tabs we are not able close it"
			notification resume: false]
	].
]

{ #category : #navigation }
ClyQueryBrowser >> spawnQueryBrowserOn: aQuery withState: navigationState [
	
	| actualQuery |
	actualQuery := aQuery isBoundToEnvironment 
		ifTrue: [ aQuery ]
		ifFalse: [ aQuery withScope: activeScope].
	
	super spawnQueryBrowserOn: actualQuery withState: navigationState
]

{ #category : #navigation }
ClyQueryBrowser >> switchResultTo: aQueryResult [

	self changeStateBy: [
		systemQuery := systemQuery withResult: aQueryResult.
		self showQueryResult
	]
]

{ #category : #navigation }
ClyQueryBrowser >> switchScopeTo: aScope [
	
	self changeStateBy: [
		systemQuery := systemQuery withScope: aScope.
		self showQueryResult]
]

{ #category : #navigation }
ClyQueryBrowser >> switchToFlatResult [
	"There is only type of default flat query result.
	If user passes query with specific flat result it should not modify default one.
	So senders and implementors will continue use same type of flat result.
	But when browser opens on query with special result it will use it localy to switch result mode"
	DefaultQueryResult := self defaultFlatQueryResult.
	self switchResultTo: expectedFlatQueryResult
]

{ #category : #navigation }
ClyQueryBrowser >> switchToHierarchicalResult [

	DefaultQueryResult := ClyHierarchicalSystemItems new.
	self switchResultTo: DefaultQueryResult
]

{ #category : #accessing }
ClyQueryBrowser >> systemQuery [
	^ systemQuery
]

{ #category : #accessing }
ClyQueryBrowser >> systemQuery: anObject [
	systemQuery := anObject
]
