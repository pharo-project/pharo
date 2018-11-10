"
I keep plugins configured for a repository. 

WARNING: Managers are kept in a weak dictionary with Repository as a key, reason why plugins can't have any kind of reference that could cause a memory leak. 
Consider using stateless plugins or instance copies of relevant information instead.
"
Class {
	#name : #IcePluginManager,
	#superclass : #Object,
	#instVars : [
		'plugins'
	],
	#category : #'Iceberg-Plugin-Base'
}

{ #category : #'instance creation' }
IcePluginManager class >> managerFor: aRepository [
	^ self basicNew initializeRepository: aRepository
]

{ #category : #'instance creation' }
IcePluginManager class >> new [
	self error: 'Use #managerFor:'
]

{ #category : #accessing }
IcePluginManager >> addPlugin: aPlugin [ 
	plugins := plugins copyWith: aPlugin
]

{ #category : #initialization }
IcePluginManager >> availablePluginsFor: aRepository [
	^ (IcePlugin allAvailableFor: aRepository) 
		collect: #new
		as: Array
]

{ #category : #'accessing ui' }
IcePluginManager >> checkoutBranchPanelsOnModel: aModel [ 

	^ self plugins
		inject: #() 
		into: [ :all :each | all, (each checkoutBranchPanelsOnModel: aModel ) ]
]

{ #category : #initialization }
IcePluginManager >> initializeRepository: aRepository. [
	self initialize.
	plugins := self availablePluginsFor: aRepository
]

{ #category : #accessing }
IcePluginManager >> plugins [ 
	^ plugins
]

{ #category : #events }
IcePluginManager >> repositoryWasCreated: aRepository [
	^ self plugins do: [ :each | each repositoryWasCreated: aRepository ]
]

{ #category : #events }
IcePluginManager >> repositoryWillBeCreated: aRepository [
	^ self plugins do: [ :each | each repositoryWillBeCreated: aRepository ]
]
