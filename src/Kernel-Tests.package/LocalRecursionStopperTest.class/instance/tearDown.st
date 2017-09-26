running
tearDown

	fork ifNotNil: [ fork terminate. fork := nil ].