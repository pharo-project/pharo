actions
validate
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := MetacelloToolBox validateConfiguration: self configurationClass ].
	^ validationResult