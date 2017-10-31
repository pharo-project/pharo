tests
testSimplefileOutput

	| fo tr |
	
	self assert: self fileReference exists not.
	
	fo := TranscriptFileOutput forFileNamed: self fileName.
	
	self assert: self fileReference exists not.
	
	tr := TranscriptStream new.
	
	tr addOutput: fo.
	
	tr show: '<Test>acbdfe</Test>'.
	
	self assert: self fileReference exists .
	
	tr close.
		
	self assert: self fileReference contents = '<Test>acbdfe</Test>'.
	
	self fileReference delete.	
	self assert: self fileReference exists not.