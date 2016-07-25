tests
testRenameClassUsingClass
	"self run: #testRenameClassUsingClass"

	self renameClassUsing: [:class :newName | class rename: newName].