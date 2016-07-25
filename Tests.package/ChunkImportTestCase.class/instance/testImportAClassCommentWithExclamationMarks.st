importing-code
testImportAClassCommentWithExclamationMarks
	| class comment commentToWrite |
	comment := 'I''m a nice comment!, treat me well :).'.
	commentToWrite := 'I''m a nice comment!!, treat me well :).' replaceAll: '!' with: '!!'.
	SystemAnnouncer uniqueInstance suspendAllWhile: [[
		class := self importAClass.
		CodeImporter evaluateString: ('!{1} commentStamp: ''<historical>'' prior: 0!{2}!' format: { class name asString . commentToWrite }).
		self assert: comment equals: class comment.
	] ensure: [ class ifNotNil: [ class removeFromSystem ] ] ]