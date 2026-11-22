"
I test the activation of ClyShowPoolUsersCommand on classes and pools.
"
Class {
	#name : 'ClyShowPoolUsersCommandTest',
	#superclass : 'TestCase',
	#category : 'Calypso-SystemTools-QueryBrowser-Tests-Tests',
	#package : 'Calypso-SystemTools-QueryBrowser-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ClyShowPoolUsersCommandTest >> testCanExecuteOnPool [

	| context |
	context := ClyFullBrowserClassContext new
		           selectedItems: { (ClyBrowserItem named: 'SharedPool' with: SharedPool) };
		           yourself.
	self assert: (ClyShowPoolUsersCommand new
			 context: context;
			 yourself) canBeExecuted
]

{ #category : 'tests' }
ClyShowPoolUsersCommandTest >> testCannotExecuteOnNonPoolClass [

	| context |
	context := ClyFullBrowserClassContext new
		           selectedItems: { (ClyBrowserItem named: 'Object' with: Object) };
		           yourself.
	self deny: (ClyShowPoolUsersCommand new
			 context: context;
			 yourself) canBeExecuted
]
