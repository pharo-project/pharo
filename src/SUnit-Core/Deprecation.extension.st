Extension { #name : 'Deprecation' }

{ #category : '*SUnit-Core' }
Deprecation >> manageTestProcessBy: aProcessMonitorTestService [
	"Deprecation is not considered as a test failure.
	So we are ignoring it here just like any other notification"
]

{ #category : '*SUnit-Core' }
Deprecation >> sunitAnnounce: aTestCase toResult: aTestResult [
	"When deprecations react as errors (the test command's --deprecations-as-errors, as the Pharo CI runs it), a deprecation reaching the test runner is an error of the test and stops it: its default action never runs, so it does not rewrite the code under test. Otherwise it goes on to its default action. A test that exercises deprecations on purpose handles them itself."

	self class reaction == ErrorDeprecationReaction ifFalse: [ ^ self pass ].
	aTestResult addError: aTestCase
]
