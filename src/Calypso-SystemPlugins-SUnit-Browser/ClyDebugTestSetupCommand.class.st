"
I am a command that debug the setup method of the selected test, then I executed the selected test method.
I install breakpoint to the start of method which opens debugger during following test run
"
Class {
	#name : 'ClyDebugTestSetupCommand',
	#superclass : 'ClyRunTestsFromMethodsCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyDebugTestSetupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $d shift meta for: self contextClass
]

{ #category : 'activation' }
ClyDebugTestSetupCommand class >> fullBrowserTableIconActivation [
	"It should not be as icon in table"
]

{ #category : 'execution' }
ClyDebugTestSetupCommand >> defaultMenuIconName [
	^ #smallDebug
]

{ #category : 'accessing' }
ClyDebugTestSetupCommand >> defaultMenuItemName [
	^'Debug test setup'
]

{ #category : 'accessing' }
ClyDebugTestSetupCommand >> description [

^ 'Debug the setup method of the selected test, then executes the selected test method.' 
]

{ #category : 'execution' }
ClyDebugTestSetupCommand >> runTest: testSelector of: testClass [

	| breakpoint |
	breakpoint := Breakpoint new
		              node: (testClass lookupSelector: #setUp) ast;
		              install.
	[ super runTest: testSelector of: testClass ] ensure: [
		[ breakpoint remove ]
			on: NotFound
			do: [ "If the test under debug has been recompiled, the breakpoint doesn't exist anymore"
				self inform: 'The breakpoint already doesn''t exist anymore' ] ]
]
