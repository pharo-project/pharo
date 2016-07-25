private
processor: aProcessor isRelevantForQuery: aString
	| trimmedProcessorTitle |
	trimmedProcessorTitle := aProcessor title asLowercase 
										copyReplaceAll: String space
										with: ''.
	^ trimmedProcessorTitle beginsWith: aString