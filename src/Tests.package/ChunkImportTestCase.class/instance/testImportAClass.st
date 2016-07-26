importing-code
testImportAClass
	| class |
	SystemAnnouncer uniqueInstance suspendAllWhile: [ [
		class := self importAClass.
		self assert: #CodeImportTestCaseTestClass equals: class name.
		self assert: (class instVarNames includes: 'var1').
		self assert: (class instVarNames includes: 'var2').
		self assert: (class instVarNames includes: 'var3').
		self assert: class category equals: 'CodeImporter-Tests-Garbage'.
		
	] ensure: [ class ifNotNil: [ class removeFromSystem ] ] ]