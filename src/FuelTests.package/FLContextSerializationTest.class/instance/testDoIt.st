tests
testDoIt
	"Serialization of DoIt methods should be possible by default.
	Note: apparently this test didn't use to terminate some times.
	I've added the #valueWithin:onTimeout: as a fallback for that."	
	| context |
	[
		[ Smalltalk evaluate: 'self error' ]
			on: Error 
			do: [:error | context := error signalerContext copyStack ].
	
		self serialize: context ]
			valueWithin: 30 seconds
			onTimeout: [ self fail ]