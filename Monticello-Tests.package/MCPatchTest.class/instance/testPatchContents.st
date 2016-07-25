tests
testPatchContents
	self assert: patch operations size = 1.
	self assert: patch operations first isModification.
	self assert: patch operations first definition selector = #one.
