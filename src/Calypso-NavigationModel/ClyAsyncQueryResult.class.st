"
I am special kind of query result which implements building in background process.

I override #rebuild method where I fork actual query execution and mark myself with ClyBackgroundProcessingTag.
When actual query is done I notify my observers to perform update.

I am created by ClyAsyncQuery as actual result instance instead of requiredResult. My #buildingQuery is actual query which I execute in the background.

Besides background processing I have other differences to my superclass logic:
- metadata is not lazy and it is built in background together with items
- built tems are never reset due to system changes. They are replaced with actual result when it is finally built.

I override #isBuilt method to detect that background processing completes and items are really built.
So you can check that async query is done using following expression: 
	asyncQuery execute isBuilt 
 
I keep reference to actual result in my #actualResult instance. So it is keep in memory as soon as I am used by somebody.

To force async query execution you need convert given query using: 
	aQuery async
	
It returns ClyAsyncQuery instance with #asyncResult variable which points to me.

I use specific logic to adopt my instances for the browser. Look at #adoptForBrowser. 
When I represent raw query result then my superclass implementation is fine. The instance of ClyQueryResultBrowserAdapter will be returned which wraps raw query items to ClyBrowserItem instances.
But If I represent a kind of ClyBrowserQueryResult then it is already adopted for the browser. But from the other side I do not provide required API of ClyBrowserQueryResult. So in that case I will return ClyAsyncBrowserQueryResultAdapter which adopts my instance to the ClyBrowserQueryResult.

Internal Representation and Key Implementation Points.

    Instance Variables
	actualResult:		<ClyQueryResult>
	buildProcess:		<Process>
"
Class {
	#name : #ClyAsyncQueryResult,
	#superclass : #ClyQueryResult,
	#instVars : [
		'actualResult',
		'buildProcess'
	],
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #accessing }
ClyAsyncQueryResult >> actualResult [
	^ actualResult
]

{ #category : #converting }
ClyAsyncQueryResult >> adoptForBrowser [
	"In case when actual query produces browser items 
	we should adopt our own interface to the interface of ClyBrowserQueryResult.
	Otherwise when actual query do not produce browser items 
	we just use superclass adapter for raw items"
	buildingQuery retrievesBrowserItems ifTrue: [ 
		^ClyAsyncBrowserQueryResultAdapter for: self ].
	
	^super adoptForBrowser
]

{ #category : #building }
ClyAsyncQueryResult >> buildActualResult [
	"The loop here prevents the race condition when actual query is just executed 
	but system is changed and this change reset built result which means nil returned from #items.
	In that case we just repeat query again"
	[actualResult := buildingQuery execute.
	(items := actualResult items) isNil] whileTrue.
	self collectMetadata.
	buildProcess := nil.
	self buildIsDone
]

{ #category : #building }
ClyAsyncQueryResult >> buildIsDone [
	self notifyObservers.
	environment systemChanged: (ClyAsyncQueryIsDone with: self)
]

{ #category : #accessing }
ClyAsyncQueryResult >> buildProcess [
	^ buildProcess
]

{ #category : #building }
ClyAsyncQueryResult >> collectMetadata [
	"In async query we always collect metadata in background 
	and full execution process completes only after this.
	So no protection is needed here and no lazy logic over metadata variable"
	metadata := ClyQueryResultMetadata new.
	environment pluginsDo: [:each | 
		buildingQuery collectMetadataOf: self by: each	]

]

{ #category : #building }
ClyAsyncQueryResult >> fillWith: queriedObjects [
	self shouldNotImplement 
]

{ #category : #building }
ClyAsyncQueryResult >> forceLazyRebuild [
	"Async result never reset own items. 
	They should keep previous values until actual result will built new one"
	
	self protectAccessWhile: [ 	
		needsRebuild := true].
]

{ #category : #'system changes' }
ClyAsyncQueryResult >> handleSystemChange: aSystemAnnouncement byProcessingList: allProcessingResults [
	| expectedResult |
	super handleSystemChange: aSystemAnnouncement byProcessingList: allProcessingResults.
	
	expectedResult := environment cachedResultOf: buildingQuery.
	expectedResult ifNil: [ ^self ].
	
	needsRebuild ifTrue: [ 
		buildProcess ifNotNil: [ 
			buildProcess terminate.
			metadata removeProperty: ClyBackgroundProcessingTag instance].
		expectedResult itemsChanged].
	
	allProcessingResults remove: expectedResult ifAbsent: [  ]
]

{ #category : #initialization }
ClyAsyncQueryResult >> initializeItems [
	"async query should never reset existing items.
	Idea to seemlessly update items when new result is ready"
	items ifNil: [super initializeItems]
]

{ #category : #testing }
ClyAsyncQueryResult >> isBuilt [
	^super isBuilt 
		and: [(self hasMetaProperty: ClyBackgroundProcessingTag) not]
]

{ #category : #accessing }
ClyAsyncQueryResult >> rawItems [
	^buildingQuery retrievesBrowserItems 
		ifTrue: [ items collect: [ :each | each actualObject ] ]
		ifFalse: [ items]
]

{ #category : #building }
ClyAsyncQueryResult >> rebuild [
	self initializeItems.	
	metadata ifNil: [metadata := ClyQueryResultMetadata new].
	metadata addProperty: ClyBackgroundProcessingTag instance.
	
	self runBuildProcess
]

{ #category : #building }
ClyAsyncQueryResult >> runBuildProcess [

	buildProcess := [self buildActualResult] newProcess.
	buildProcess 
		name: 'Build result of ', buildingQuery printString;
		priority: Processor userBackgroundPriority.
	buildProcess resume
]
