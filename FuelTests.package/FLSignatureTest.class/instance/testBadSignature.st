tests
testBadSignature

	serializer signature: 'FUELx'.
	materializer signature: 'FUELy'.
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadSignature
		whoseDescriptionIncludes: 'FUELy'
		description: 'The materializer should raise an error when signature differs of the expected one.'