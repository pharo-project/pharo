Class {
	#name : #ClyInstallMetaLinkPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter',
		'node',
		'metalink',
		'metalink2',
		'breakpoint',
		'executionCounter',
		'watchpoint'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Tests-Metalinks'
}

{ #category : #running }
ClyInstallMetaLinkPresenterTest >> presenterForMetalinkInstallation [
	^ClyInstallMetaLinkPresenter onNode: node forInstallation:  true
]

{ #category : #running }
ClyInstallMetaLinkPresenterTest >> presenterForMetalinkUninstallation [
	^ ClyInstallMetaLinkPresenter onNode: node forInstallation: false
]

{ #category : #running }
ClyInstallMetaLinkPresenterTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	MetaLink uninstallAll.
	node := RBTemporaryNode named: 'test'.
	node propertyAt: #links put: OrderedCollection new.
	metalink := MetaLink new.
	metalink2 := MetaLink new.
	breakpoint := MetaLink new metaObject: Break; yourself.
	executionCounter := MetaLink new metaObject: ExecutionCounter new; yourself.
	watchpoint := MetaLink new metaObject: Watchpoint new; yourself.
	node links add: metalink.
	node links add: breakpoint.
	node links add: executionCounter.
	node links add: watchpoint
]

{ #category : #running }
ClyInstallMetaLinkPresenterTest >> tearDown [
	MetaLink uninstallAll
]

{ #category : #tests }
ClyInstallMetaLinkPresenterTest >> testRelevantMetalinksForInstallation [
	|links|
	links := self presenterForMetalinkInstallation allRelevantMetaLinks.
	self assert: links size >= 2.
	self assertCollection: links includesAll: { metalink. metalink2 }.
	self denyCollection: links includesAll: { breakpoint. watchpoint. executionCounter  }.
	
]

{ #category : #tests }
ClyInstallMetaLinkPresenterTest >> testRelevantMetalinksForUninstallation [
	| links |
	links := self presenterForMetalinkUninstallation allRelevantMetaLinks.
	self assert: links size equals: 1.
	self assertCollection: links hasSameElements: { metalink }
]

{ #category : #tests }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsCollectionSize [
	self assert: self presenterForMetalinkInstallation toolbarButtons size equals: 2.
	self assert: self presenterForMetalinkUninstallation toolbarButtons size equals: 2.
]

{ #category : #tests }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsForMetalinkInstallation [
	|toolbarButtons|
	toolbarButtons := self presenterForMetalinkInstallation toolbarButtons.
	self assert: toolbarButtons first label equals: 'Install'.
	self assert: toolbarButtons last label equals: 'Cancel'.
]

{ #category : #tests }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsForMetalinkUninstallation [
	|toolbarButtons|
	toolbarButtons := self presenterForMetalinkUninstallation toolbarButtons.
	self assert: toolbarButtons first label equals: 'Uninstall'.
	self assert: toolbarButtons last label equals: 'Cancel'.
]
