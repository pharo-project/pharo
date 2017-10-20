reporting
reportSpaceAnalysis: aStream

	spaceAnalysisResult ifNil: [
		(UIManager default
			confirm: 'Running the space analysis\might take a few seconds.' withCRs
			label: 'System Reporter')
				ifFalse: [
					categoriesSelected remove: #'Space Analysis'.
					^ self changed: #categorySelected ].
		
		spaceAnalysisResult := String streamContents: [ :stream |
			SpaceTally new printSpaceAnalysis: 1 onStream: stream ] ].
	self header: 'Space Analysis' on: aStream.
	aStream nextPutAll: spaceAnalysisResult; cr