tests
testRenameToMakesMCDirty
	| package |
	
	package := (RPackage named: #'Test1') register.
	self createNewClassNamed: 'TestClass' inCategory: 'Test1'.
	
	package renameTo: 'Test2'.
	
	