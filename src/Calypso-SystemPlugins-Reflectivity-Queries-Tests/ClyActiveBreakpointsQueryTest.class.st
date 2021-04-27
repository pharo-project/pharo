Class {
	#name : #ClyActiveBreakpointsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#instVars : [
		'installedBreakpoints'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #running }
ClyActiveBreakpointsQueryTest >> createQuery [
	^ClyActiveBreakpointsQuery from: ClyClassScope of: ClyClassWithHaltsMock in: environment
]

{ #category : #running }
ClyActiveBreakpointsQueryTest >> installBreakpointInto: aMethod [
	| breakpoint |
	breakpoint := Breakpoint new node: aMethod ast; always.
	installedBreakpoints add: breakpoint.
	breakpoint install
]

{ #category : #running }
ClyActiveBreakpointsQueryTest >> setUp [
	super setUp.
	
	installedBreakpoints := OrderedCollection new.
	ClyClassWithHaltsMock compile: 'methodWithHalts1 self halt'
]

{ #category : #running }
ClyActiveBreakpointsQueryTest >> tearDown [
	installedBreakpoints do: [ :each | each remove ].
	installedBreakpoints removeAll.
	ClyClassWithHaltsMock removeSelector: #methodWithHalts1.
	super tearDown
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testCheckIfEmpty [
	self installBreakpointInto: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints).
	query scope: (ClyMethodScope 
		of: (ClyClassWithHaltsMock >> #methodWithHalts1) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithBreakpointsMock >> #methodWithoutBreakpoints) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testComparison [
	self assert: ClyActiveBreakpointsQuery new equals: ClyActiveBreakpointsQuery new.

	self assert: ClyActiveBreakpointsQuery newIncludingHaltsForTesting equals: ClyActiveBreakpointsQuery newIncludingHaltsForTesting.

	self deny: ClyActiveBreakpointsQuery new equals: ClyActiveBreakpointsQuery newIncludingHaltsForTesting
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testDecoratingMethodWithBreakpoint [

	| plugin method methodItem |
	plugin := ClyReflectiveEnvironmentPlugin new.
	self installBreakpointInto: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints).
	method := ClyClassWithBreakpointsMock >> #methodWithBreakpoints.
	methodItem := method asCalypsoBrowserItem.
		
	plugin decorateBrowserItem: methodItem ofMethod: method.
	
	self assert: (methodItem isMarkedWith: ClyMethodWithBreakpointTag).
	self deny: (methodItem isMarkedWith: ClyMethodWithHaltTag).
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testDecoratingMethodWithHalt [

	| plugin method methodItem |
	plugin := ClyReflectiveEnvironmentPlugin new.
	method := ClyClassWithHaltsMock >> #methodWithHalts1.
	methodItem := method asCalypsoBrowserItem.
		
	plugin decorateBrowserItem: methodItem ofMethod: method.
	
	self assert: (methodItem isMarkedWith: ClyMethodWithHaltTag).
	self deny: (methodItem isMarkedWith: ClyMethodWithBreakpointTag)
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testFromThreeMethods [
	self installBreakpointInto: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints).
	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithHaltsMock >> #methodWithHalts1. 
		ClyClassWithBreakpointsMock >> #methodWithBreakpoints.
		ClyClassWithBreakpointsMock >> #methodWithoutBreakpoints}.
	
	self assert: foundSelectors sorted asArray equals: #(#methodWithBreakpoints #methodWithHalts1) 
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testNotSelectsMethodWithoutBreakpoint [
	self installBreakpointInto: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints).

	self deny: (query selectsMethod: ClyClassWithBreakpointsMock >> #methodWithoutBreakpoints)
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testSelectsMethodWithBreakpoint [
	self installBreakpointInto: (ClyClassWithBreakpointsMock >> #methodWithBreakpoints).

	self assert: (query selectsMethod: ClyClassWithBreakpointsMock >> #methodWithBreakpoints)
]

{ #category : #tests }
ClyActiveBreakpointsQueryTest >> testSelectsMethodWithHalt [
	
	self assert: (query selectsMethod: ClyClassWithHaltsMock>>#methodWithHalts1)
]
