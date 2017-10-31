helper methods
removeFileNamed: aFileName from: aTranscript
	
	| output |
	output := aTranscript outputs
		detect: [ :each | 	each class == self and: [ each fileName = aFileName ] ] 
		ifFound: [ :each | each ]
		ifNone: [ ^self ].
	
	aTranscript removeOutput: output.