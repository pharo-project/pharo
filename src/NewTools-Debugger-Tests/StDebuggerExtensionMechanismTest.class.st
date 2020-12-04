Class {
	#name : #StDebuggerExtensionMechanismTest,
	#superclass : #TestCase,
	#instVars : [
		'debugger',
		'oldExtensionActivation'
	],
	#category : #'NewTools-Debugger-Tests-Presenters'
}

{ #category : #accessing }
StDebuggerExtensionMechanismTest >> debugger [
	^ debugger ifNil: [ debugger := self debuggerClass basicNew ]
]

{ #category : #accessing }
StDebuggerExtensionMechanismTest >> debuggerClass [
	^ StDebugger
]

{ #category : #running }
StDebuggerExtensionMechanismTest >> setUp [

	super setUp.
	oldExtensionActivation := self debuggerClass activateDebuggerExtensions.
	StDebuggerExtensionGeneratorForTests generateTestClass
]

{ #category : #running }
StDebuggerExtensionMechanismTest >> tearDown [

	StDebuggerExtensionGeneratorForTests removeTestClass.
	self debuggerClass activateDebuggerExtensions: oldExtensionActivation.
	super tearDown
]

{ #category : #'tests - extensions' }
StDebuggerExtensionMechanismTest >> testExtensionToolsClasses [

	self
		assertCollection: StDebugger extensionToolsClasses
		equals: TStDebuggerExtension users
]

{ #category : #'tests - extensions' }
StDebuggerExtensionMechanismTest >> testHasAnyActivatedExtensions [

	| testClass |
	testClass := StDebuggerExtensionGeneratorForTests testClass.
	self deny: (self debuggerClass hasAnyActivatedExtension: { testClass }).
	testClass showInDebugger: true.
	self assert: (self debuggerClass hasAnyActivatedExtension: { testClass })
]

{ #category : #'tests - extensions' }
StDebuggerExtensionMechanismTest >> testInstantiateExtensionToolsPage [
	|dbg toolPage|
	dbg := self debugger.
	toolPage := dbg instantiateExtensionToolsPage: StDummyDebuggerPresenter.
	self assert: toolPage class identicalTo: SpNotebookPage.
	self assert: toolPage presenterProvider value class identicalTo: StDummyDebuggerPresenter.
	self assert: toolPage title equals: StDummyDebuggerPresenter new debuggerExtensionToolName.
	self assertCollection: dbg extensionTools includesAll: { toolPage presenterProvider value}.
	 
]

{ #category : #'tests - extensions' }
StDebuggerExtensionMechanismTest >> testRegisterExtensionTool [
	|dbg object|
	dbg := self debugger.
	self assertEmpty: dbg extensionTools.
	dbg registerExtensionTool: (object := Object new).
	self assert: dbg extensionTools size equals: 1.
	self assertCollection: dbg extensionTools equals: { object } asOrderedCollection 
]

{ #category : #'tests - extensions' }
StDebuggerExtensionMechanismTest >> testUsesExtensions [
	| testClass |
	testClass := StDebuggerExtensionGeneratorForTests testClass.	
	testClass showInDebugger: true.
	self debuggerClass activateDebuggerExtensions: false.
	self deny: self debuggerClass usesExtensions.
	self debuggerClass activateDebuggerExtensions: true.
	self assert: self debuggerClass usesExtensions
]
