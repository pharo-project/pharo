support
testRenaming
	"self debug: #testRenaming"

	"self run: #testRenaming"

	| oldName newMetaclassName class |
	oldName := className.
	newMetaclassName := (renamedName , #' class') asSymbol.
	class := Smalltalk globals at: oldName.
	class class compile: 'dummyMeth'.
	class rename: renamedName.
	self assert: class name = renamedName.
	self assert: (ChangeSet current changedClassNames includes: renamedName).
	self assert: (ChangeSet current changedClassNames includes: newMetaclassName)