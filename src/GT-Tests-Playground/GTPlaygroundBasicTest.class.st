Class {
	#name : #GTPlaygroundBasicTest,
	#superclass : #TestCase,
	#instVars : [
		'playground',
		'window'
	],
	#category : #'GT-Tests-Playground'
}

{ #category : #running }
GTPlaygroundBasicTest >> setUp [
	super setUp.
	playground := GTPlayground new.
]

{ #category : #running }
GTPlaygroundBasicTest >> tearDown [
	window ifNotNil: [ window delete ].
	window := nil.
	(RPackageOrganizer default packageNamed: 'GTMockTests' ifAbsent: [ "No package to delete" ]) ifNotNil: #removeFromSystem.
	playground := nil.
	super tearDown
]

{ #category : #running }
GTPlaygroundBasicTest >> testAccessBindings [
	|obtainedBindings expectedBindings|

	expectedBindings := Dictionary new.  
	expectedBindings add: (WorkspaceVariable key: #a value: 1).
	expectedBindings add: (WorkspaceVariable key: #b value: 2).
		
	window := playground openOn: (GTPlayPage new saveContent: 'a:=1. b:=a+1').
	playground inspectorPresentation firstPresentation selectLine.
	playground inspectorPresentation firstPresentation evaluateSelection.
		
	obtainedBindings := playground currentBindings.
		
	self assert: obtainedBindings equals: expectedBindings.
	
]

{ #category : #running }
GTPlaygroundBasicTest >> testPageActionsIn [
	| actions selector action |
	selector := playground class
		compile:
			'mockMethodPageAction
	<pageActionOrder: 10>
	^ GLMGenericAction new
		action: [ :presentation | self inform: ''page action works'' ];
		iconNamed: #abstract;
		title: ''A mock page action'''
		classified: '*GTMockTests'.
		
	[ self assert: selector notNil.
	window := playground openOn: (GTPlayPage new saveContent: 'a:=1. b:=a+1').
	actions := playground pageActions.
	self deny: actions size isZero.
	action := actions detect: [ :eachAction | eachAction title = 'A mock page action' ].
	self deny: action isNil ]
		ensure: [ playground class removeSelector: selector ]
]

{ #category : #running }
GTPlaygroundBasicTest >> testPlaygroundActionsIn [
	| actions selector action |
	selector := playground class
		compile:
			'mockMethodPlaygrounAction
	<playgroundActionOrder: 100>
	^ GLMGenericAction new
		action: [ :presentation | self inform: ''playground action works'' ];
		iconNamed: #abstract;
		title: ''A mock playground action'''
		classified: '*GTMockTests'.
		
	[ self assert: selector notNil.
	window := playground openOn: (GTPlayPage new saveContent: 'a:=1. b:=a+1').
	actions := playground playgroundActions.
	self deny: actions size isZero.
	action := actions detect: [ :eachAction | eachAction title = 'A mock playground action' ].
	self deny: action isNil ]
		ensure: [ playground class removeSelector: selector ]
]

{ #category : #running }
GTPlaygroundBasicTest >> testResetBindings [
	|obtainedBindings|

	window := playground openOn: (GTPlayPage new saveContent: 'number+1').
	playground 
		setBindings: { #number -> 2 } asDictionary.
		
	obtainedBindings := playground currentBindings.
	self assert: obtainedBindings size equals: 1.

	playground resetBindings.		
	
	obtainedBindings := playground currentBindings.
	self assert: obtainedBindings size equals: 0.
	
]

{ #category : #running }
GTPlaygroundBasicTest >> testSettingBindings [
	|obtainedResult|
	obtainedResult:=nil.
	window := playground openOn: (GTPlayPage new saveContent: 'number+1').
	playground 
		setBindings: { #number -> 2 } asDictionary.
	playground inspectorPresentation firstPresentation selectLine.
	
	playground inspectorPresentation firstPresentation announce: (GLMEvaluateSelection new 
		action: [ :aResult | obtainedResult := aResult ];
		makeSyncronous).
		
	self assert: obtainedResult equals: 3
	
]
