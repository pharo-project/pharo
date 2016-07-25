tests
testAsOriginIdentifier
	| id |
	id := GLMPortIdentifier new.
	self assert: id asGlamourOriginIdentifier equals: id