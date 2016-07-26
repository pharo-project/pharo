tests
testTrackerUrl
	self assert: issue trackerUrl equals: ('https://pharo.fogbugz.com/default.asp?', issueNumber asString) asUrl