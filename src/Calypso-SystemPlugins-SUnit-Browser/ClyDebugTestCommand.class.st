"
I am a command to debug selected test method.
I install breakpoint to the start of method which opens debugger during following test run
"
Class {
	#name : 'ClyDebugTestCommand',
	#superclass : 'ClyRunTestsFromMethodsCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyDebugTestCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $d meta for: self contextClass
]

{ #category : 'activation' }
ClyDebugTestCommand class >> fullBrowserTableIconActivation [
	"It should not be as icon in table"
]

{ #category : 'execution' }
ClyDebugTestCommand >> defaultMenuIconName [
	^ #smallDebug
]

{ #category : 'accessing' }
ClyDebugTestCommand >> defaultMenuItemName [
	^'Debug tests'
]

{ #category : 'accessing' }
ClyDebugTestCommand >> description [

^ 'Debug selected test method' 
]

{ #category : 'execution' }
ClyDebugTestCommand >> runTest: testSelector of: testClass [

	| breakpoint |
	breakpoint := Breakpoint new
		              node: (testClass lookupSelector: testSelector) ast;
		              install.
	[ super runTest: testSelector of: testClass ] ensure: [
		[ breakpoint remove ]
			on: NotFound
			do: [ "If the test under debug has been recompiled, the breakpoint doesn't exist anymore"
				self inform: 'The breakpoint already doesn''t exist anymore' ] ]
]
