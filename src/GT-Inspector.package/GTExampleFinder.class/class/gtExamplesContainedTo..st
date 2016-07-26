public
gtExamplesContainedTo: examples
	Object withAllSubclasses do: [ :class | 
		class gtExamplesFinder ifNotNil: [ :finder | 
			self = finder class ifTrue: [ 
				finder gtExamplesContainedTo: examples ] ] ]