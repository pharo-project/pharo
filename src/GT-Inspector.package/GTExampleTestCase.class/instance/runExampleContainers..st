private
runExampleContainers: aCollection
	| allExamples |
	(allExamples := aCollection gather: [ :container | container gtExamplesContained ])
		do: [ :example | self runExample: example ]
		displayingProgress: 'Running ' , allExamples size asString , ' Examples'