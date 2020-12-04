Class {
	#name : #StDebuggerContextPredicateTest,
	#superclass : #TestCase,
	#instVars : [
		'predicate'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #running }
StDebuggerContextPredicateTest >> setUp [
	super setUp.
	
	predicate := (StDebuggerContextPredicate context: 0)
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testContext [
	self assert: predicate context equals: 0
]

{ #category : #tests }
StDebuggerContextPredicateTest >> testIsSteppable [
	
	self assert: predicate isSteppable.
	predicate postMortem: true.
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
