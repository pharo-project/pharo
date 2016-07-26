mapping
add: anObject name: globalName selector: selector traceWith: anAnalysis

	self add: anObject traceWith: anAnalysis.
	globalSends
		at: anObject
		ifAbsentPut: [ Association key: globalName value: selector ]