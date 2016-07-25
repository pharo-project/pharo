accessing
validate
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := MetacelloToolBox validateProject: self project version: self version versionString ].
	^ validationResult