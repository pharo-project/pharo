"
I am not a subclass of MCRepositoryTest because my purpose was to show that a bug had been fixed and my author didn't feel like implementing all the reuirements of MCRepositoryTests
"
Class {
	#name : #MCSubDirectoryRepositoryTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests'
}

{ #category : #tests }
MCSubDirectoryRepositoryTest >> testIssue7368 [
	"This is not a usage example. Hopefully the ugly private method under test will be refactored out of existence soon ;)"

	| collection |
	collection := MCSubDirectoryRepository new splitNameVersionExtensionFor: 'Spec-Core-AuthorName.120.mcz'.
	self assert: collection first equals: 'Spec-Core-AuthorName'.
	self assert: collection second equals: 120.
	self assert: collection third equals: 'mcz'.
]
