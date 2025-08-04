Class {
	#name : 'ClyInstallMetaLinkPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'node',
		'metalink',
		'metalink2',
		'breakpoint',
		'executionCounter',
		'watchpoint'
	],
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Tests-Metalinks',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser-Tests',
	#tag : 'Metalinks'
}

{ #category : 'helper methods' }
ClyInstallMetaLinkPresenterTest >> dummyMethod [
	self
]

{ #category : 'helper methods' }
ClyInstallMetaLinkPresenterTest >> nodeInRealMethod [
	^(self class >> #dummyMethod) ast statements first
]

{ #category : 'running' }
ClyInstallMetaLinkPresenterTest >> presenterForMetalinkInstallation [
	^ClyMetaLinkInstallationPresenter onNode: node forInstallation:  true
]

{ #category : 'running' }
ClyInstallMetaLinkPresenterTest >> presenterForMetalinkUninstallation [
	^ ClyMetaLinkInstallationPresenter onNode: node forInstallation: false
]

{ #category : 'running' }
ClyInstallMetaLinkPresenterTest >> setUp [
	super setUp.
	MetaLink uninstallAll.
	node := OCVariableNode named: 'test'.
	metalink := MetaLink new.
	metalink2 := MetaLink new.
	breakpoint := MetaLink new metaObject: Break; yourself.
	executionCounter := MetaLink new metaObject: ExecutionCounter new; yourself.
	watchpoint := MetaLink new metaObject: Watch new; yourself.
	node propertyAt: #links put: {metalink. breakpoint. executionCounter. watchpoint} asOrderedCollection
]

{ #category : 'running' }
ClyInstallMetaLinkPresenterTest >> tearDown [
	metalink uninstall.
	metalink2 uninstall.
	super tearDown
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testInstallSelectedMetalink [
	|presenter list|
	presenter := ClyMetaLinkInstallationPresenter onNode: self nodeInRealMethod forInstallation:  true.
	list := presenter metalinkListPresenter.
	list clickItem: 1.
	self deny: self nodeInRealMethod hasMetaLinks.
	presenter installSelectedMetalink.
	self assert: self nodeInRealMethod hasMetaLinks.
	self assert: self nodeInRealMethod links asArray first identicalTo: list items first
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testInstallSelectedMetalinkActionButton [
	|presenter list|
	presenter := ClyMetaLinkInstallationPresenter onNode: self nodeInRealMethod forInstallation:  true.
	list := presenter metalinkListPresenter.
	list clickItem: 1.
	self deny: self nodeInRealMethod hasMetaLinks.
	presenter toolbarButtons first click.
	self assert: self nodeInRealMethod hasMetaLinks.
	self assert: self nodeInRealMethod links asArray first identicalTo: list items first
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testRelevantMetalinksForInstallation [
	|links|
	links := self presenterForMetalinkInstallation allRelevantMetaLinks.
	self assert: links size >= 2.
	self assertCollection: links includesAll: { metalink. metalink2 }.
	self denyCollection: links includesAll: { breakpoint. watchpoint. executionCounter  }
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testRelevantMetalinksForUninstallation [
	| links |
	links := self presenterForMetalinkUninstallation allRelevantMetaLinks.
	self assert: links size equals: 1.
	self assertCollection: links hasSameElements: { metalink }
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsCollectionSize [
	self assert: self presenterForMetalinkInstallation toolbarButtons size equals: 2.
	self assert: self presenterForMetalinkUninstallation toolbarButtons size equals: 2
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsForMetalinkInstallation [
	|toolbarButtons|
	toolbarButtons := self presenterForMetalinkInstallation toolbarButtons.
	self assert: toolbarButtons first label equals: 'Install'.
	self assert: toolbarButtons last label equals: 'Cancel'
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testToolbarButtonsForMetalinkUninstallation [
	|toolbarButtons|
	toolbarButtons := self presenterForMetalinkUninstallation toolbarButtons.
	self assert: toolbarButtons first label equals: 'Uninstall'.
	self assert: toolbarButtons last label equals: 'Cancel'
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testUninstallSelectedMetalink [
	|presenter list|
	self nodeInRealMethod link: metalink.
	presenter := ClyMetaLinkInstallationPresenter onNode: self nodeInRealMethod forInstallation:  false.
	list := presenter metalinkListPresenter.
	list clickItem: 1.
	self assert: self nodeInRealMethod hasMetaLinks.
	presenter uninstallSelectedMetalink.
	self deny: self nodeInRealMethod hasMetaLinks
]

{ #category : 'tests' }
ClyInstallMetaLinkPresenterTest >> testUninstallSelectedMetalinkActionButton [
	|presenter list|
	self nodeInRealMethod link: metalink.
	presenter := ClyMetaLinkInstallationPresenter onNode: self nodeInRealMethod forInstallation:  false.
	list := presenter metalinkListPresenter.
	list clickItem: 1.
	self assert: self nodeInRealMethod hasMetaLinks.
	presenter toolbarButtons first click.
	self deny: self nodeInRealMethod hasMetaLinks
]
