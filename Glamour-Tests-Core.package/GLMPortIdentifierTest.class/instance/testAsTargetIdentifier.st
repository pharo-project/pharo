tests
testAsTargetIdentifier
	| id |
	id := GLMPortIdentifier new.
	self assert: id asGlamourTargetIdentifier equals: id