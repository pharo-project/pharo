testing
testExtractHelpContentsFromMarkedString
	|content|
	content := builder extractHelpContentFromString: 'outer<help>inner</help>outer'.
	self assert: content = 'inner'.
	