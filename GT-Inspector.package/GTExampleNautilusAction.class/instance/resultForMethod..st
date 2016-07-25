private
resultForMethod: aCompiledMethod
	^ aCompiledMethod gtExamples 
		ifNotEmpty: [ :examples | examples first resultCached ]
		ifEmpty: [ nil ]