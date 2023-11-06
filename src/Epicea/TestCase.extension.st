Extension { #name : 'TestCase' }

{ #category : '*Epicea' }
TestCase class >> shouldLogWithEpicea [
	"When running a test, we disable the loggings of Epicea to run the tests silently. In case a TestCase explicitly wants to Epicea logs, then I can be overriden to return true and enable the logs."

	^ false
]

{ #category : '*Epicea' }
TestCase >> shouldLogWithEpicea [

	^ self class shouldLogWithEpicea
]
