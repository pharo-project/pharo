testing
testFileOutResult
	| suite result fileout |
	suite := self classForTestSuite new.
	suite
		addTest: (self class selector: #noop);
		addTest: (self class selector: #fail);
		addTest: (self class selector: #error).
	result := suite run.
	fileout := String streamContents: [ :aStream | result fileOutOn: aStream ].
	self
		assert: fileout
		equals:
			'3 run, 1 passes, 0 skipped, 0 expected failures, 1 failures, 1 errors, 0 unexpected passes
Failures:
SUnitTest(TestAsserter)>>#fail

Errors:
SUnitTest>>#error
'