tests
doTestExportLiteral: original 
	| exported |
	exported := self export: original.
	self assert: exported equals: original.