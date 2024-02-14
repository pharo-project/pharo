Class {
	#name : 'ReRenameMethodDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReRenameMethodDriverTest >> setUpDriver: driver [

	super setUpDriver: driver.
	
	driver methodNameEditorPresenterClass: StMethodNameEditorPresenterMock
]

{ #category : 'tests' }
ReRenameMethodDriverTest >> testInvalidNameFollowedByAValidName [

	| rbclasses driver |
	self skip.
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	driver := ReRenameMethodDriver new
					scopes: {} model: rbclasses renameMethodSignature: #accessing in: MyClassARoot.

	self setUpDriver: driver.
	
	" this is unclear that the methodname presenter should be a mock class. 
	It looks more complex. "
	
	
	
	
	"requestDialog := MockObject new.
	 openOn: methodName
		          withInvalidArgs: (RBCondition invalidArgumentNamesForSelector: originalMessage selector in: model)
		          canRenameArgs: self canRenameArgs
		          canRemoveArgs: self canRemoveArgs
		          canAddArgs: self canAddArgs
		          canEditName:
	requestDialog on: #openModal respond: #unusedVar2.
	requestDialog on: #openModal respond: #okName.
	requestDialog on: #title: respond: 'nothing'.
	requestDialog on: #text: respond: 'nothing'.
	driver requestDialog: requestDialog."
	
	"it like a smoke test and for the moment we cannot control better what happen."
	"driver runRefactoring"
]
