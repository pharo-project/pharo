protected
setDefaultMaterialization

	materializationFactory := [:aDecoder | 
		(FLMaterialization 
			with: aDecoder)
			run; 
			yourself ]
	
	
