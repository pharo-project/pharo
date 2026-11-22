"
Debug the setup method of the selected test, then executes the selected test method.

I install breakpoint to the start of method which opens debugger during following test run
"
Class {
	#name : 'ClyDebugTestSetupCommand',
	#superclass : 'ClyRunTestsFromMethodsCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'accessing' }
ClyDebugTestSetupCommand >> iconName [
	^ #smallDebug
]

{ #category : 'accessing' }
ClyDebugTestSetupCommand >> name [
	^'Debug test setup'
]

{ #category : 'execution' }
ClyDebugTestSetupCommand >> runTest: testSelector of: testClass [

	| breakpoint |
	breakpoint := DebugPointManager installNew: BreakDebugPoint on: (testClass lookupSelector: #setUp) ast.
	[ super runTest: testSelector of: testClass ] ensure: [
		[ breakpoint remove ]
			on: NotFound
			do: [ "If the test under debug has been recompiled, the breakpoint doesn't exist anymore"
				self inform: 'The breakpoint already doesn''t exist anymore' ] ]
]
