Class {
	#name : #StDebuggerInspectorTest,
	#superclass : #TestCase,
	#instVars : [
		'inspector',
		'session'
	],
	#category : #'NewTools-Debugger-Tests-Presenters'
}

{ #category : #running }
StDebuggerInspectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	inspector := StDebuggerInspector new
]

{ #category : #running }
StDebuggerInspectorTest >> tearDown [
	session ifNotNil:[session clear].
	super tearDown
]

{ #category : #tests }
StDebuggerInspectorTest >> testDefaultLayoutSpec [
	
	self assert: inspector currentLayoutSpec equals: inspector defaultLayoutSpec
]

{ #category : #tests }
StDebuggerInspectorTest >> testSetAssertionLayoutSpec [
	|inspectorLayout expectedLayout|
	inspector setAssertionLayout. 
	inspectorLayout := inspector layout.
	expectedLayout := inspector class assertionFailureSpec.
	self assert: inspector currentLayoutSpec equals: inspector assertionLayoutSpec.		
	self assertCollection: inspectorLayout children equals: expectedLayout children. 
	self assert: inspectorLayout direction equals: expectedLayout direction.
	self assert: inspectorLayout class identicalTo: expectedLayout class
]

{ #category : #tests }
StDebuggerInspectorTest >> testSetDefaultLayoutSpec [
	|inspectorLayout expectedLayout|
	inspector setDefaultLayout. 
	inspectorLayout := inspector layout.
	expectedLayout := inspector class defaultSpec.
	self assert: inspector currentLayoutSpec equals: inspector defaultLayoutSpec.
	self assertCollection: inspectorLayout children equals: expectedLayout children. 
	self assert: inspectorLayout direction equals: expectedLayout direction.
	self assert: inspectorLayout class identicalTo: expectedLayout class
]

{ #category : #tests }
StDebuggerInspectorTest >> testUpdateLayoutForContextsWithAssertionFailure [
	| debugger assertionFailure |
	debugger := StTestDebuggerProvider new
		debuggerWithFailingAssertionContext.
	session := debugger session.
	inspector
		updateLayoutForContexts: session interruptedContext
		isAssertionFailure: true.
	assertionFailure := inspector assertionFailure.
	self
		assert: inspector currentLayoutSpec
		equals: inspector assertionLayoutSpec.
	self
		assert: assertionFailure actualObject
		equals: ((session interruptedContext tempNamed: #aStringOrBlock) at: 2).
	self
		assert: assertionFailure expectedObject
		equals: ((session interruptedContext tempNamed: #aStringOrBlock) at: 1)
]

{ #category : #tests }
StDebuggerInspectorTest >> testUpdateLayoutForContextsWithoutAssertionFailure [
	| debugger |
	debugger := StTestDebuggerProvider new
		debuggerWithFailingAssertionContext.
	session := debugger session.
	inspector
		updateLayoutForContexts: session interruptedContext
		isAssertionFailure: false.
	self
		assert: inspector currentLayoutSpec
		equals: inspector defaultLayoutSpec.
	self assert: inspector assertionFailure equals: nil
]
