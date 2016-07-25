tests
testBadVersion

	serializer version: 2.
	materializer version: 1.
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadVersion
		whoseDescriptionIncludes: '2'
		description: 'The materializer should raise an error when version differs of the expected one.'