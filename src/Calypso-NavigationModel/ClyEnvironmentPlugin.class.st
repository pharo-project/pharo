"
I am a root of environment plugins hierarchy.
My subclasses are responsible for three things:

1) Extend browser items with arbitrary properties. Any plugin can compute specific properties for given item. 
For example plugin can mark class that it is abstract:
	
	MyPluginClass>>decorateBrowserItem: anItem ofClass: aClass
		aClass isAbstract ifTrue: [ anItem markWith: ClyAbstractItemTag  ]
		
Concrete decoration methods which plugin can implement are depends on the item types which plugin is going support. For another example to decorate methods plugin should implement: 

- decorateBrowserItem: anItem ofMethod: aMethod

Normally when you develop navigation over concrete system you build some common superclass plugin which will provide stubs for all item types form your domain. 
In case of Smalltalk navigation there is ClySystemEnvironmentPlugin which implements empty methods to decorate packages, classes and methods (like in examples)

2) Connect external systems to the navigation environment. 
Plugin package can provide new kind of queries and scopes which retrieves new kind of items. These items can be from systems which are external to the main navigation environment. 
Plugin should take care about external updates by subscribing to external system for the changes and by delegating them to the navigation environment.

For example SUnit plugin extends Smalltalk environment with information about tests. It subscribes on SUnit events about test ran. And when it happens it delegates event to the environment which updates methods queries with new information about test result.

To connect to external system plugin implements following method:
- attatchToSystem 
Here plugin can subscribe on external events. For example SUnit plugin subscribes on ""TestCase historyAnnouncer"" to know when user run tests.

And to disconnect from system should implement:
- detatchFromSystem
For example SUnit plugin unsubscribes from ""TestCase historyAnnouncer"".

3) Collect query result metadata. 
For example SUnit plugin can collect how many success tests are retrieved by method query.
The exact methods which plugin should implement depends on the kind of queries it is supposed to support. In case of SUnit it implements:
- collectMetadataOfMethods: aQueryResult

But decision what method to use is responsibility of queries. For example: 

	ClyMethodQuery>>collectMetadataOf: aQueryResult by: anEnvironmentPlugin
		anEnvironmentPlugin collectMetadataOfMethods: aQueryResult

As in case of items decoration the superclass of plugins can provide empty methods for metadata collection. So concrete plugin only chooses what it wants.

That is all responsibility.
 
To activate plugin it should be added to navigation environment: 
	environment addPlugin: anEnvironmentPlugin 

Default global environment adds all plugins automatically. Only plugins marked as auto-activated are used (which is true by default):
	
	ClyEnvironmentPlugin class>>isAutoActivated
		^isAutoActivated ifNil [true]

You can disable any plugin using: 
	
	ClyConcretePlugin disable

But it do not affect default navigation environment. You will need recreate it.

 
Internal Representation and Key Implementation Points.

    Instance Variables
	environment:		<ClyNavigationEnvironment>
"
Class {
	#name : #ClyEnvironmentPlugin,
	#superclass : #Object,
	#instVars : [
		'environment'
	],
	#classInstVars : [
		'isAutoActivated'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #initialization }
ClyEnvironmentPlugin class >> disable [
	isAutoActivated := false
]

{ #category : #initialization }
ClyEnvironmentPlugin class >> enable [
	isAutoActivated := true
]

{ #category : #initialization }
ClyEnvironmentPlugin class >> initialize [
	self isAbstract ifTrue: [ ^self ].
	
	ClyNavigationEnvironment installNewPlugin: self
]

{ #category : #testing }
ClyEnvironmentPlugin class >> isAbstract [
	"Subclasses existence is simplest logic to think that plugin is abstract. It is supposed to be common case for plugin implementation.
	In specific cases just override this message if you want plugin with subclasses be not abstract"
	^self subclasses notEmpty
]

{ #category : #testing }
ClyEnvironmentPlugin class >> isAutoActivated [
	^isAutoActivated ifNil: [ true ] 
]

{ #category : #testing }
ClyEnvironmentPlugin class >> isAutoActivatedOn: aNavigationEnvironment [
	^self isAbstract not & self isAutoActivated 
		and: [ self isRelatedTo: aNavigationEnvironment system ]
]

{ #category : #testing }
ClyEnvironmentPlugin class >> isRelatedTo: aSystem [
	"My subclasses should decide what kind of system they are related to"
	
	self subclassResponsibility 
]

{ #category : #controlling }
ClyEnvironmentPlugin >> attachToSystem [
]

{ #category : #'item decoration' }
ClyEnvironmentPlugin >> decorateBrowserItem: anItem ofObject: anObject [
]

{ #category : #controlling }
ClyEnvironmentPlugin >> detachFromSystem [
]

{ #category : #accessing }
ClyEnvironmentPlugin >> environment [
	^ environment
]

{ #category : #accessing }
ClyEnvironmentPlugin >> environment: anObject [
	environment := anObject
]
