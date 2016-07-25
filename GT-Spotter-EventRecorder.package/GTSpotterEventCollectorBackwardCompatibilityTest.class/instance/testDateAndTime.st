tests
testDateAndTime
	self versionsDo: [ :announcement :version |
		announcement unpackedData do: [ :eachEvent | | dateAndTime |
			dateAndTime := eachEvent dateAndTime.
			self assert: dateAndTime class equals: DateAndTime ] ]