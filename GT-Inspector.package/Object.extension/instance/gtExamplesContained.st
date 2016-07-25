*GT-Inspector-Examples-UI
gtExamplesContained
	^ [ self gtExamples ]
		on: Error
		do: [ #() ]