Class {
	#name : #ClyAccrossWindowNavigationStateTest,
	#superclass : #ClyNavigationHistoryTestCase,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #running }
ClyAccrossWindowNavigationStateTest >> restoreBrowserInstance [

	self snapshotState.
	
	browser := navigationState createBrowser
]

{ #category : #running }
ClyAccrossWindowNavigationStateTest >> snapshotState [

	navigationState := ClyAccrossWindowNavigationState from: browser
		
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testCreatesBrowserWithNavigationEnvironment [
	
	self restoreBrowserInstance.
	
	self assert: browser navigationEnvironment equals: environment
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testCreatesBrowserWithNavigationHistory [
	
	self restoreBrowserInstance.
	
	self assert: browser navigationHistory equals: navigationState navigationHistory
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testCreatesBrowserWithPlugins [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self restoreBrowserInstance.
	
	self assert: browser plugins first browser equals: browser.
	self assert: browser plugins first class equals: ClyStandardBrowserPlugin.
	self deny: navigationState browserPlugins first == browser plugins first
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testPluginsWithoutBrowser [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self snapshotState.
	
	self assert: navigationState browserPlugins first browser isNil
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testRetrievesBrowserEnvironment [

	self snapshotState.
	
	self assert: navigationState navigationEnvironment equals: environment
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testRetrievesBrowserState [

	self snapshotState.
	
	self assert: (navigationState browserState isKindOf: ClyBrowserState )
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testRetrievesNavigationHistory [
	
	self snapshotState.
	
	self assert: navigationState navigationHistory equals: browser navigationHistory
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTest >> testRetrievesPlugins [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self snapshotState.
	
	self assert: navigationState browserPlugins first class equals: ClyStandardBrowserPlugin
]
