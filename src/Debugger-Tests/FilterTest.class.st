"
SUnit test for debugger filters
"
Class {
	#name : #FilterTest,
	#superclass : #TestCase,
	#instVars : [
		'context',
		'process',
		'session',
		'nilContext',
		'otherProcess',
		'otherSession'
	],
	#category : #'Debugger-Tests'
}

{ #category : #running }
FilterTest >> setUp [
	super setUp.
	context := [ (Set with: 1 with: 2) collect: [ :e | e * 2 ]. self halt ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
	session := process newDebugSessionNamed: 'test session' startedAt: context.
	nilContext := [ self isNil ] asContext.
	otherProcess := Process 
		forContext: nilContext 
		priority: Processor userInterruptPriority.
	otherSession := otherProcess newDebugSessionNamed: 'other session' startedAt: nilContext.
	otherSession stepInto
]

{ #category : #tests }
FilterTest >> testBlockFilter [
	| fromBlockFilter |
	
	fromBlockFilter := [ :ctx | ctx isNotNil ] asFilter.
	self assert: (fromBlockFilter shouldDisplay: context)
]

{ #category : #tests }
FilterTest >> testBooleanFilter [
	| kernelFilter doItFilter filters booleanFilter |
	
	kernelFilter := KernelClassesFilter new.
	doItFilter := SelectorFilter forSelector: #doIt.
	filters := OrderedCollection with: kernelFilter with: doItFilter.
	booleanFilter := BooleanFilter withFilters: filters operator: #or:.
	self assert: (booleanFilter shouldDisplay: context)
]

{ #category : #tests }
FilterTest >> testKernelClassesFilter [
	| kernelClassesFilter setContext |
	
	setContext := session stepInto stepInto stack first.
	kernelClassesFilter := KernelClassesFilter new.
	self assert: (kernelClassesFilter shouldDisplay: context).
	self assert: (kernelClassesFilter shouldDisplay: nilContext).
	self deny: (kernelClassesFilter shouldDisplay: setContext)
]

{ #category : #tests }
FilterTest >> testSelectorFilter [
	| doItFilter nilMessagesFilter |
	
	doItFilter := SelectorFilter forSelector: #doIt.
	self assert: (doItFilter shouldDisplay: context).
	self assert: (doItFilter shouldDisplay: nilContext).
	nilMessagesFilter := SelectorFilter forSelectors: (OrderedCollection with: #isNil with: #ifNil:).
	self assert: (nilMessagesFilter shouldDisplay: context)	"self deny: (nilMessagesFilter shouldDisplay: otherSession context)"
]
