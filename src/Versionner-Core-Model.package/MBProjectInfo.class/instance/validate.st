accessing
validate
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := self validateIfAbsent: [ #() ] ].
	^ validationResult