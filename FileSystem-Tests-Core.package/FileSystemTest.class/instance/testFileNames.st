tests
testFileNames
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) ]
		ensure: [ reference delete ] ]