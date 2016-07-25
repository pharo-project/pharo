private
basicRunFailures
	" self new runFailures "

	(allFailures := self readFailuresFromFile: (FileSystem workingDirectory / 'gtExamplesAllFailures.txt')) 
		do: [ :each |
			each gtExamples do: [ :example |
				self runExample: example ] ]
		displayingProgress: 'Running ', allFailures size asString, ' failures ...'