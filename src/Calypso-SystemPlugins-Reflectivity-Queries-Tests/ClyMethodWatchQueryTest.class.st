Class {
	#name : #ClyMethodWatchQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#instVars : [
		'installedWatchpoints'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Watchpoints'
}

{ #category : #running }
ClyMethodWatchQueryTest >> createQuery [
	^ClyMethodWatchQuery from: ClyClassScope of: self class in: environment
]

{ #category : #running }
ClyMethodWatchQueryTest >> installWatchInto: selector [
	| watchpoint |
	watchpoint := Watch in: (self class >> selector) ast sendNodes last.
	installedWatchpoints add: watchpoint.
	^watchpoint
]

{ #category : #running }
ClyMethodWatchQueryTest >> methodWithWatch [
	"Currently watchpoint is only compatible with message nodes.
	So we can't install it directly on method node"
	^1+2
]

{ #category : #running }
ClyMethodWatchQueryTest >> methodWithoutWatch [
]

{ #category : #running }
ClyMethodWatchQueryTest >> setUp [
	super setUp.
	
	installedWatchpoints := OrderedCollection new
]

{ #category : #running }
ClyMethodWatchQueryTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyReflectiveEnvironmentPlugin new
]

{ #category : #running }
ClyMethodWatchQueryTest >> tearDown [
	installedWatchpoints do: [ :each | each uninstall ].
	installedWatchpoints removeAll.
	
	super tearDown
]

{ #category : #tests }
ClyMethodWatchQueryTest >> testCheckIfEmpty [
	self installWatchInto: #methodWithWatch.
	query scope: (ClyMethodScope 
		of: (self class >> #methodWithWatch) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithBreakpointsMock >> #methodWithoutBreakpoints) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMethodWatchQueryTest >> testDecoratingMethodWithExecutionCounter [

	| plugin method methodItem |
	plugin := ClyReflectiveEnvironmentPlugin new.
	self installWatchInto: #methodWithWatch.
	method := self class >> #methodWithWatch.
	methodItem := method asCalypsoBrowserItem.
		
	plugin decorateBrowserItem: methodItem ofMethod: method.
	
	self assert: (methodItem isMarkedWith: ClyMethodWithWatchTag)
]

{ #category : #tests }
ClyMethodWatchQueryTest >> testFromTwoMethods [
	self installWatchInto: #methodWithWatch.
	self queryFromScope: ClyMethodScope ofAll: {
		self class >> #methodWithWatch. 
		self class >> #methodWithoutWatch}.
	
	self assert: foundSelectors sorted asArray equals: #(#methodWithWatch) 
]

{ #category : #tests }
ClyMethodWatchQueryTest >> testNotSelectsMethodWithoutWatch [

	self deny: (query selectsMethod: self class >> #methodWithoutWatch)
]

{ #category : #tests }
ClyMethodWatchQueryTest >> testSelectsMethodWithWatch [
	self installWatchInto: #methodWithWatch.

	self assert: (query selectsMethod: self class >> #methodWithWatch)
]
