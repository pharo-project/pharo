tests - converting
testTrimLineSpaces
	self assert: 'test     \do' withCRs trimLineSpaces equals: 'test\do' withCRs.
	self assert: 'test     \  do' withCRs trimLineSpaces equals: 'test\  do' withCRs.
	self assert: 'test \do\' withCRs trimLineSpaces equals: 'test\do' withCRs.
	self assert: 'test \do' withCRs trimLineSpaces equals: 'test\do' withCRs.
	self assert: 'test do' withCRs trimLineSpaces equals: 'test do' withCRs.
	self assert: 'test\ \ ' withCRs trimLineSpaces equals: 'test\\' withCRs.