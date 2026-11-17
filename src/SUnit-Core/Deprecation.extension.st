Extension { #name : 'Deprecation' }

{ #category : '*SUnit-Core' }
Deprecation >> manageTestProcessBy: aProcessMonitorTestService [
	"Deprecation is not considered as a test failure.
	So we are ignoring it here just like any other notification"
]

{ #category : '*SUnit-Core' }
Deprecation >> sunitAnnounce: aTestCase toResult: aTestResult [
	"On the Pharo CI, a deprecation reaching the test runner is an error of the test and stops it: it does not go unnoticed, and its default action never runs, so it does not rewrite the code under test. Elsewhere it goes on as a warning. A test that exercises deprecations on purpose handles them itself."

	aTestCase onPharoCITestingEnvironment ifFalse: [ ^ self pass ].
	aTestResult addError: aTestCase
]
