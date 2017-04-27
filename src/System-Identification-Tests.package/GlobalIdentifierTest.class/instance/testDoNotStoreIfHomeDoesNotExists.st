tests
testDoNotStoreIfHomeDoesNotExists
	"Ignore persistence if HOME directory does not exists.
	In the case that the image is deployed on a server, HOME directory may not exists.
	See issue https://pharo.fogbugz.com/f/cases/19944."
	| strategy |
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		strategy checker: (self newChecker answer: false; yourself).
		strategy := strategy previous.
	].
	identifier ensure.
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		self deny: strategy preferences exists.
		strategy := strategy previous.
	].
	self assert: identifier computerUUID notNil.
