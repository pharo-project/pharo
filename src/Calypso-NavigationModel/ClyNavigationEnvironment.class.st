"
I am environment where user navigates over particular system by quering objects from particular scope of other objects. 
I cache query results and extend retrieved objects using builtin plugin system.

My instances should be created over some system: 

	environment := ClyNavigationEnvironment over: aSystem

I do not provide any requirement for the class of target system. It can be any object from particular domain which represents the global view on it.
But Calypso requires two things which system should implement. It can define them as extension inside Calypso integration package:

1) The system should provide global scope which will adopt it to Calypso navigation model. This scope will represent the root place where all objects in system can be reached.
Concrete system should implement subclass of ClySystemScope and return it instance as global scope of actual system object. It should be returned from method #asGlobalScopeIn: which system should implement.
Users can access the system and it scope from my instance:
	
	environment system
	environment systemScope

2) The system should notify my instance about changes.
I maintain query cache and it requires invalidation when system is changed. So responsibility of any system is to subscribe and unsubscribe my instances for possible changes: 
	
	aSystem subscribe: environment
	aSystem unsubscribe: environment
	
I am subscribed when I am attaching to the system:
	
	environment attachToSystem
	
And I am unsubscribed when detaching: 

	environment detachFromSystem
	
User should manually attach my instances to the system when they are interested in updates.

I should not be subclassed. 

Users can maintain singleton instance of me for concrete system. It will provide global cache for all queries over given system.
For this purpose I provide class side variable defaultGlobalEnvironments. It maps system instance to my instance (aNavigationEnvironment) over it.
Idea that applications usually have kind of default system instance (imaging Smalltalk global). And to provide default navigation over it users can extend my class side with appropriate accessing method. 
For example navigation over current Smalltalk image is accessed using #currentImage method: 

	ClyNavigationEnvironment class>>currentImage 	
		^self defaultOver: ClySystemEnvironment currentImage

The method #defaultOver: retrieves existing instance or creates new one. Such default instances are automatically attached to the system.
In this example the system is an instance of ClySystemEnvironment which represents Smalltalk system. (it was suitable to introduce extra wrapper for this domain. Look it comment for details).

To reset all global environments send #reset message to me: 

	ClyNavigationEnvironment reset
	
It will detach (unsubscribe) environments from their systems and clear collection.

I provide #query: method to execute queries using cache. But userrs should not use it directly.
Users should prepare scope instance using given environment:

	scope := ClyClassScope of: Object in: environment 

And then create and execute query instance: 

	query := ClyAllMethods from: scope.
	query execute

Underhood my #query: method is called by scope during query execution logic. I returns existing result if it exists. Otherwise I build new one.

There is another method which allow to check that given query will produce empty result:
	
	environment isQueryEmpty: query
	
It also should not be used directly. But query should be asked directly: 

	query hasEmptyResult

If there is existing result then I just check that it is not empty. Otherwise I ask query to #checkEmptyResult where it evaluates actual logic.

To maintain query cache I use two mutexes:

- accessGuard, protects any modification of queryCache
- updateGuard, protects cache updating in the way that multiple changes will be always processed in sequence

In my cache the query is a key and the result is a value. The cache is weak and unused result is cleaned by GC. In same time unused keys (queries which result is cleaned) are collected when new query is executed (using #cleanGarbageInCache).

I am extendable by plugins, subclasses of ClyEnvironmentPlugin. Plugins are responsible to extend queries of particular system in following ways:

- plugin can extend properties of query result (ClyBrowserItem instances and query metadata).
- plugin can supply information about other systems. 
Plugins are packaged separatelly. Plugin package can extend visibility of existing scopes by providing new information from external systems. This information can be retrieved by new queries.
And to manage this information plugin should notify environment about external changes.

To add plugin use following method:

	environment addPlugin: MyTestPlugin new.
	
And to access plugins use: 
	
	environment pluginsDo: aBlock
	
When I am attached to the system I also attach all my plugins to it using:

	plugin attachToSystem

In this method plugin is able to subscribe on own system changes to notify environment about them.
 
Internal Representation and Key Implementation Points.

    Instance Variables 
	plugins:		<Collection of<ClyEnvironmentPlugin>>>
	queryCache:		<WeakValueDictionary of<ClyQuery, ClyQueryResult>>
	system: 	<Object>
	updateStrategy:	<ClyEnvironmentUpdateStrategy>
	updateGuard:	<Mutex>
	accessGuard:	<Mutex>
"
Class {
	#name : #ClyNavigationEnvironment,
	#superclass : #Object,
	#instVars : [
		'plugins',
		'updateStrategy',
		'accessGuard',
		'updateGuard',
		'system',
		'queryCache'
	],
	#classInstVars : [
		'defaultGlobalEnvironments'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #default }
ClyNavigationEnvironment class >> defaultOver: aSystem [
	defaultGlobalEnvironments ifNil: [ defaultGlobalEnvironments := IdentityDictionary new ].
	
	^defaultGlobalEnvironments at: aSystem ifAbsentPut: [ 
		(ClyNavigationEnvironment over: aSystem)
			setUpAvailablePlugins;
			attachToSystem ]
]

{ #category : #default }
ClyNavigationEnvironment class >> installNewPlugin: anEnvironmentPluginClass [
	defaultGlobalEnvironments ifNil: [ ^self ].
	
	defaultGlobalEnvironments do: [ :each | 
		(anEnvironmentPluginClass isAutoActivatedOn: each)
			ifTrue: [ each addPlugin: anEnvironmentPluginClass new]]
]

{ #category : #'instance creation' }
ClyNavigationEnvironment class >> over: aSystem [
	^self new 
		system: aSystem
]

{ #category : #default }
ClyNavigationEnvironment class >> reset [
	"It will reset all caches and unsubscribe from system environment"
	<script>
	defaultGlobalEnvironments ifNil: [ ^self ].
	defaultGlobalEnvironments do: #detachFromSystem.
	defaultGlobalEnvironments := nil
]

{ #category : #accessing }
ClyNavigationEnvironment >> addPlugin: anEnvironmentPlugin [
	plugins detect: [ :each | each class = anEnvironmentPlugin class ] ifFound: [ ^self ].
	
	anEnvironmentPlugin environment: self.
	plugins add: anEnvironmentPlugin
]

{ #category : #'system changes' }
ClyNavigationEnvironment >> announceChangesOf: aQueryResult [
	
	updateStrategy announceChangesOf: aQueryResult
]

{ #category : #'system subscription' }
ClyNavigationEnvironment >> attachToSystem [
	system subscribe: self.
	plugins do: [:each | each attachToSystem]
]

{ #category : #accessing }
ClyNavigationEnvironment >> cachedResultOf: aQuery [

	^queryCache at: aQuery ifAbsent: [ nil ]
]

{ #category : #cleaning }
ClyNavigationEnvironment >> cleanGarbageInCache [

	accessGuard critical: [ 
		queryCache clyCleanGarbage]
]

{ #category : #'system subscription' }
ClyNavigationEnvironment >> detachFromSystem [
	system unsubscribe: self.
	plugins do: [ :each | each detachFromSystem ]
]

{ #category : #accessing }
ClyNavigationEnvironment >> getPlugin: environmentPluginClass [
	^plugins detect: [ :each | each class = environmentPluginClass ] 
]

{ #category : #accessing }
ClyNavigationEnvironment >> getPlugin: environmentPluginClass ifAbsent: absentBlock [
	^plugins detect: [ :each | each class = environmentPluginClass ] ifNone: absentBlock
]

{ #category : #'system changes' }
ClyNavigationEnvironment >> handleSystemChange: aSystemAnnouncement [

	| todoList todoSize anyResult |
	todoList := 	(queryCache values select: [ :each | each notNil ]) as: IdentitySet.
	todoSize := 0.
	[[todoSize = todoList size] whileFalse: [
		todoSize := todoList size.  
		todoList asArray do: [ :eachResult |
			(todoList includes: eachResult) ifTrue: [  
				eachResult handleSystemChange: aSystemAnnouncement byProcessingList: todoList]]].
	todoList notEmpty] whileTrue: [ 
		anyResult := todoList anyOne.
		anyResult handleSystemChange: aSystemAnnouncement.
		todoList remove: anyResult]
]

{ #category : #initialization }
ClyNavigationEnvironment >> initialize [
	super initialize.
	
	accessGuard := Mutex new.
	updateGuard := Mutex new.
	plugins := OrderedCollection new.
	queryCache := WeakValueDictionary new.
	updateStrategy := ClyInstantEnvironmentUpdateStrategy new
]

{ #category : #queries }
ClyNavigationEnvironment >> isQueryEmpty: aQuery [
	| result |
	result := queryCache at: aQuery ifAbsent: [ nil ].	"cache is weak dict where ifAbsentPut: not works"
	result ifNotNil: [ 
		result isBuilt ifTrue: [ ^result isEmpty ]].
	^ aQuery checkEmptyResult
]

{ #category : #accessing }
ClyNavigationEnvironment >> plugins [
	^plugins
]

{ #category : #accessing }
ClyNavigationEnvironment >> pluginsDo: aBlock [
	^plugins do: aBlock
]

{ #category : #queries }
ClyNavigationEnvironment >> query: aQuery [

	| result |
	self cleanGarbageInCache.
	result := queryCache at: aQuery ifAbsent: [nil]. "cache is weak dict where ifAbsentPut: not works"
	result ifNil: [  
		result := aQuery prepareNewResult.
		aQuery fixStateBeforeExecution.
		"We should ensure that state of query will not be modified after execution 
		because it is the key in cache.
		So aQuery is supposed to become readonly object together with required internal state"
		accessGuard critical: [ queryCache at: aQuery put: result]].	
	result rebuildIfNeeded.
	^result
]

{ #category : #accessing }
ClyNavigationEnvironment >> queryCache [
	^queryCache
]

{ #category : #queries }
ClyNavigationEnvironment >> querySystemFor: aTypedQuery [ 
	aTypedQuery bindTo: self systemScope in: self.
	^aTypedQuery execute
]

{ #category : #initialization }
ClyNavigationEnvironment >> setUpAvailablePlugins [

	ClyEnvironmentPlugin allSubclasses 
		select: [ :each | each isAutoActivatedOn: self ]
		thenDo: [ :each | self addPlugin: each new ]
]

{ #category : #accessing }
ClyNavigationEnvironment >> system [
	^ system
]

{ #category : #accessing }
ClyNavigationEnvironment >> system: anObject [
	system := anObject
]

{ #category : #'system changes' }
ClyNavigationEnvironment >> systemChanged: aSystemAnnouncement [

	self updateUsing: ClyFullEnvironmentUpdateStrategy new by: [
		self handleSystemChange: aSystemAnnouncement 
	]
]

{ #category : #queries }
ClyNavigationEnvironment >> systemScope [
	^ system asGlobalScopeIn: self
]

{ #category : #'system changes' }
ClyNavigationEnvironment >> updateUsing: newUpdateStrategy by: updateBlock [
	"here is special logic to break current mutex when any error is signalled.
	Without this logic fixing code inside spawned debugger will lead to deadlock
	because applying method changes will blocked at this accessGuard.
	Breaking mutex is done by creating new one which meand that current process
	is not guarded any more and proceeding execution in debugger can lead to some errors in rare cases. But it is less problem than locked UI"
	[
		updateGuard critical: [ | oldStrategy |
			oldStrategy := updateStrategy.
			[
				updateStrategy := newUpdateStrategy.
				updateBlock on: Error do: [ :err |
					updateStrategy := oldStrategy.
					updateGuard := Mutex new.
					err pass]
			] ensure: [ 
				updateStrategy == newUpdateStrategy ifTrue: [ 
					"In case of error another process can set up new strategy. 
					This condition will avoid possible collision"
					updateStrategy := oldStrategy ]]]
	] ensure: [ newUpdateStrategy publishCollectedChanges]
]

{ #category : #accessing }
ClyNavigationEnvironment >> withCachedQueriesDo: aBlock [

	queryCache select: [ :each | each notNil ] thenDo: aBlock
]
