private
withName: name extension: extension
	| basename |
	basename :=String streamContents:
		[:out |
		out nextPutAll: name.
		out nextPut: self extensionDelimiter.
		out nextPutAll: extension].
	^ self copy 
		at: self size put: basename;
		yourself