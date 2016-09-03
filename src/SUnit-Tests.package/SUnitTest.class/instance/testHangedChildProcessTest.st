testing
testHangedChildProcessTest

	| case result hangedProcess |

	case := self class selector: #hangedChildProcessTest.	
	result := case run.

	self
		assertForTestResult: result
		runCount: 1
		passed: 1
		failed: 0
		errors: 0.
		
	hangedProcess := Process allInstances 
		detect: [: each | each name = #hangedChildProcessTest] ifNone: [ ^self ].
	self assert: hangedProcess isTerminated