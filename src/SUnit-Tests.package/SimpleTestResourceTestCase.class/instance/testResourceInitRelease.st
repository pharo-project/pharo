not categorized
testResourceInitRelease
	| suite error failure |
	suite := self classForTestSuite new.
	suite addTest: (error := self class selector: #error).
	suite addTest: (failure := self class selector: #fail).
	suite addTest: (self class selector: #dummy).
	suite run.
	self assert: resource hasSetup