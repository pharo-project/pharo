Class {
	#name : #ClyAccrossWindowNavigationStateTests,
	#superclass : #ClyNavigationHistoryTestCase,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #running }
ClyAccrossWindowNavigationStateTests >> restoreBrowserInstance [

	self snapshotState.
	
	browser := navigationState createBrowser
]

{ #category : #running }
ClyAccrossWindowNavigationStateTests >> snapshotState [

	navigationState := ClyAccrossWindowNavigationState from: browser
		
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testCreatesBrowserWithNavigationEnvironment [
	
	self restoreBrowserInstance.
	
	self assert: browser navigationEnvironment equals: environment
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testCreatesBrowserWithNavigationHistory [
	
	self restoreBrowserInstance.
	
	self assert: browser navigationHistory equals: navigationState navigationHistory
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testCreatesBrowserWithPlugins [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self restoreBrowserInstance.
	
	self assert: browser plugins first browser equals: browser.
	self assert: browser plugins first class equals: ClyStandardBrowserPlugin.
	self deny: navigationState browserPlugins first == browser plugins first
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testPluginsWithoutBrowser [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self snapshotState.
	
	self assert: navigationState browserPlugins first browser isNil
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testRetrievesBrowserEnvironment [

	self snapshotState.
	
	self assert: navigationState navigationEnvironment equals: environment
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testRetrievesBrowserState [

	self snapshotState.
	
	self assert: (navigationState browserState isKindOf: ClyBrowserState )
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testRetrievesNavigationHistory [
	
	self snapshotState.
	
	self assert: navigationState navigationHistory equals: browser navigationHistory
]

{ #category : #tests }
ClyAccrossWindowNavigationStateTests >> testRetrievesPlugins [

	browser addPlugin: ClyStandardBrowserPlugin new.
	
	self snapshotState.
	
	self assert: navigationState browserPlugins first class equals: ClyStandardBrowserPlugin
]
