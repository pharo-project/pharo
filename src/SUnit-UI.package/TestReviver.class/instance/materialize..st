private
materialize: fuelFileReference
	fuelFileReference ifNil: [ ^self ].
	FLMaterializer materializeFromFileNamed: fuelFileReference fullName