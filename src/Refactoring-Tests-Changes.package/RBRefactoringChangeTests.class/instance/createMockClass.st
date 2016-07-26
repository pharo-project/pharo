mocking
createMockClass
	self class compiler evaluate: 'Object subclass: #RBRefactoringChangeMock
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''''
	category: ''Refactoring-Tests-Changes'''.
	
	self class compiler evaluate:  'RBRefactoringChangeMock class
	instanceVariableNames: ''classInstVar'''.
	
	self changeMock compile: 'one ^ 1' classified: 'accessing'.