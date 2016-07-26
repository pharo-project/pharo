tests
testFromNullReferenceDetectNotFound

	self
		should: [ log from: log nullReference detect: [ :entry | true ] ]
		raise: KeyNotFound