importing-code
testImportAClassComment
	| class comment |
	comment := 'I''m a nice comment, treat me well :).'.
	SystemAnnouncer uniqueInstance suspendAllWhile: [[
		class := self importAClass.
		CodeImporter evaluateString: ('!{1} commentStamp: ''<historical>'' prior: 0!{2}!' format: { class name asString . comment }).
		self assert: comment equals: class comment.
	] ensure: [ class ifNotNil: [ class removeFromSystem ] ] ]