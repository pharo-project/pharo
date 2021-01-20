Class {
	#name : #StDebuggerContextPredicateTest,
	#superclass : #TestCase,
	#instVars : [
		'predicate',
		'context'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #running }
StDebuggerContextPredicateTest >> setUp [
	super setUp.
	context := [  ] asContext.
	predicate := (StDebuggerContextPredicate context: context)
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testContext [
	self assert: predicate context identicalTo: context
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testIsContextDead [
	
	self deny: predicate isContextDead.
	context pc: nil.
	self assert: predicate isContextDead
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testIsSteppable [

	self assert: predicate isSteppable.
	predicate postMortem: true.
	self deny: predicate isSteppable.

	predicate postMortem: false.
	self assert: predicate isSteppable.
	context pc: nil.
	self assert: context isDead.
	self deny: predicate isSteppable
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testPrintDescription [
	self skip.
	self assert: predicate printDescription equals: '0'
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testPrintHaltDescription [
	|haltContext|
	haltContext := (StTestDebuggerProvider new debuggerWithRunnableContext) interruptedContext.
	predicate context: haltContext.
	self assert: predicate printDescription equals: 'Halt in ', haltContext printString
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testPrintPostMortemDescription [
	self skip.
	predicate postMortem: true.
	self assert: predicate printDescription equals: '[Post-mortem] 0'
]
