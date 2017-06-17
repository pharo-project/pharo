testing
isValid
	self flag: #todo. "Make this tests more solid"
	^ self isMissing not
		and: [ self isCodeMissing not ]