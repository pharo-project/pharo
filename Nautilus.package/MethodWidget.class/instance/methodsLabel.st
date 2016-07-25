private
methodsLabel

	^ self showInstance
		ifTrue: ['Instance methods' asText ]
		ifFalse: ['Class methods' asText allBold ]