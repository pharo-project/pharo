tests
testIssue7368
	"This is not a usage example. Hopefully the ugly private method under test will be refactored out of existence soon ;)"

	| collection |
	collection := MCSubDirectoryRepository new splitNameVersionExtensionFor: 'Spec-Core-AuthorName.120.mcz'.
	self assert: collection first equals: 'Spec-Core-AuthorName'.
	self assert: collection second equals: 120.
	self assert: collection third equals: 'mcz'.