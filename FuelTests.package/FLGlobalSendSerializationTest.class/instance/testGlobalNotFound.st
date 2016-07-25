tests
testGlobalNotFound

	self should: [ self resultOfSerializeAndMaterialize: FLGlobalSendNotPresentMock new ]
		raise: FLGlobalNotFound
		whoseDescriptionIncludes: #FLKeyNotPresentInSmalltalks 
		description: 'Should raise an error when global name is not present in Smalltalks globals.' 
	
