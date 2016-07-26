private
basicLoadFromPublishUrl: aString
	^ GTPlayPage new 
		saveContent: (GTUrlProvider new get: aString)
		yourself