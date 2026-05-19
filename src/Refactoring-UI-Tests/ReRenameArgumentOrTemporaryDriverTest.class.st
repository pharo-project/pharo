Class {
	#name : 'ReRenameArgumentOrTemporaryDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReRenameArgumentOrTemporaryDriverTest >> testGatherUserInputUsesInjectedDialog [

	| method driver dialogMock |
	method := ReClassToConvertTemporaryToInstanceVariable >> #doAction1.
	driver := ReRenameArgumentOrTemporaryDriver new.
	driver
		sourceNode: (OCVariableNode new
				 name: 'temp';
				 start: 15;
				 yourself);
		method: method;
		scopes: { RBBrowserEnvironment default }.

	dialogMock := StRequestDialogMock new.
	dialogMock response: 'tmp2'.
	driver requestDialog: dialogMock.

	driver
		forTesting: true;
		runRefactoring.
	self assert: driver requestDialog isNotNil.
	self assert: driver requestDialog ~= driver defaultRequestDialog
]
